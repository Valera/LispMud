;;; core-zone.lisp

(in-package :cl-user)
(defpackage :lispmud/core-zone
  (:use :cl)
  (:import-from :iter #:iter #:for #:collect #:nconcing #:with #:repeat)
  (:import-from :alexandria #:remove-from-plistf #:random-elt)
  (:import-from :bt #:make-lock)
  (:import-from :lispmud/core-utils #:pvalue #:name)
  (:import-from :lispmud/player #:output)
  (:import-from :lispmud/mob #:make-mob-from-plist #:standard-mob
                #:schedule-mob-events #:banker)
  (:import-from :lispmud/core-room #:description #:short-description
                #:dest-room #:east-exit #:west-exit #:north-exit #:south-exit
                #:editor-info #:place-type #:myroom #:reverse-direction
                #:dx-for-direction #:dy-for-direction #:*exits* #:direction
                #:exit #:exit-slot-for-direction #:mobs #:triggers))
(in-package :lispmud/core-zone)

(defvar *zone-id-source* 0
  "Источник идентификаторов для зон. После создания каждой зоны увеличивается на 1.")

(defclass zone ()
  ((name :accessor name :initarg :name)
   (symbol :accessor zone-symbol :initarg :zone-symbol)
   (id :reader id)
   (map-array :accessor map-array :initarg :map-array)
   (entry-rooms :accessor entry-rooms :initarg :entry-rooms)
   (mobs-spec :accessor mobs-spec :initarg :mobs-spec)
   (mobs-counters :accessor mobs-counters)
   (mobs-max-numbers :accessor mobs-max-numbers)
   (lock :accessor lock)
   (message-queue :accessor message-queue :initform (sb-queue:make-queue :name "zone-queue"))))

(defmethod initialize-instance :after ((zone zone) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value zone 'id) *zone-id-source*)
  (incf *zone-id-source*)
  (setf (lock zone) (make-lock (concatenate 'string "Zone mutex: " (name zone)))))

(defmethod zone-symbol ((zone symbol))
  zone)

(defun save-zone (zone stream)
  (let ((map-array (map-array zone)))
    (write-sequence
     (jonathan:to-json
      (list :zone-symbol (zone-symbol zone) :zone-name (name zone) :mobs-spec (mobs-spec zone)
            :zone-size (array-dimensions map-array)
            :zone-rooms
            (iter (for y from 0 below (array-dimension map-array 0))
	      (nconcing
	       (iter (for x from 0 below (array-dimension map-array 1))
		 (for r = (aref map-array y x))
		 (when r
		   (with-accessors ((west-exit west-exit) (east-exit east-exit)
				    (south-exit south-exit) (north-exit north-exit))
		       r          ;(west-exit east-exit north-exit south-exit) r
		     (flet ((exit-sexpr (room-on-exit)
			      (when room-on-exit
				(list :exit-description (short-description (dest-room room-on-exit)) :door :null))))
		       (collect (list :room-class (or (getf (editor-info r) :room-class)
						      (class-name (class-of r)))
				      :room-short-description (short-description r)
				      :room-long-description (description r)
				      :room-flags ()
				      :room-type (place-type r)
				      :coord (list x y)
				      :west-exit (or (exit-sexpr west-exit) :null)
				      :east-exit (or (exit-sexpr east-exit) :null)
				      :north-exit (or (exit-sexpr north-exit) :null)
				      :south-exit (or (exit-sexpr south-exit) :null)))))))))))
     stream)
    (force-output stream)))

(defun convert-to-keyword (js-name)
  (if (keywordp js-name)
      js-name
      (intern js-name :keyword)))

(defun maybe-convert-to-keyword (js-name)
  (or (find-symbol (string-upcase js-name) :keyword)
      js-name))

(defun convert-single-float-to-double-in-tree (tree-or-leaf)
  (typecase tree-or-leaf
    (list (mapcar #'convert-single-float-to-double-in-tree tree-or-leaf))
    (single-float (coerce tree-or-leaf 'double-float))
    (t tree-or-leaf)))

(defun load-zone (filename)
  (with-open-file (stream filename)
    ;; TODO: fix style-warning in the next line and report wrong zone format properly
    (destructuring-bind (&key zone-symbol zone-name zone-rooms mobs-spec
			      ((:zone-size (size-x size-y))))
	(let* ((s (alexandria:read-file-into-string filename))
               (parsed (yason:parse s :object-key-fn #'maybe-convert-to-keyword :object-as :plist)))
          (convert-single-float-to-double-in-tree parsed))
      (iter
        (for one-mob-spec in mobs-spec)
        (setf (getf one-mob-spec :type) (convert-to-keyword (getf one-mob-spec :type))))
      (setf zone-symbol (convert-to-keyword zone-symbol))
      (pvalue zone-name zone-rooms (list size-x size-y) mobs-spec)
      (let ((map (make-array (list size-x size-y) :initial-element nil))
	    (entry-rooms nil))
	(dolist (room zone-rooms)
	  (pvalue room)
          ;; TODO: fix style-warning in the next line and report wrong zone format properly
	  (destructuring-bind
		(&key ((:coord (x y)))
		      room-class room-short-description room-long-description room-flags room-type
		      west-exit east-exit north-exit south-exit)
	      room
            (setf room-class (find-symbol room-class :lispmud/core-room))
            (setf room-type (maybe-convert-to-keyword room-type))
	    (assert (subtypep room-class 'myroom))
	    (setf (aref map y x)
		  (make-instance room-class :short-description room-short-description
				 :description room-long-description
				 :place-type room-type :flags room-flags
				 :west-exit west-exit :east-exit east-exit
				 :north-exit north-exit :south-exit south-exit))
	    (push (aref map y x) entry-rooms)))
	(link-rooms map)
	(make-instance 'zone :name zone-name :zone-symbol zone-symbol :map-array map
		       :mobs-spec mobs-spec :entry-rooms entry-rooms)))))

(defun link-rooms (zone-map)
  (pvalue "link-rooms" zone-map)
  (iter
    (for (y x room) in-matrix zone-map)
    (if room
	(dolist (direction *exits*)
	  (if (exit room direction)
	      (let ((x1 (+ x (dx-for-direction direction)))
		    (y1 (+ y (dy-for-direction direction)))
		    room1)
		(if (and (array-in-bounds-p zone-map y1 x1)
			 (setf room1 (aref zone-map y1 x1))
			 (exit room1 (reverse-direction direction)))
		    (progn
		      (setf (slot-value room (exit-slot-for-direction direction))
			    (make-instance 'exit :dest-room room1)))
		    (progn
		      (pvalue x y x1 y1 room1)
		      (format t "Warning: ~a exit in room (~a, ~a) doen't have pair~%"
			      direction x y)))))))))

(defun unlink (room)
  "Удалить все входы в комнату и выходы из неё"
  (iter (for i in *exits*)
	(when (exit room i)
	  (setf (exit (dest-room (exit room i)) (reverse-direction i)) nil)
	  (setf (exit room i) nil))))

(defun unlink-orphaned-rooms (zone)
  (iter (with room-list = (iter (for (y x room) in-matrix (map-array zone))
				(when room
				  (collect room))))
	(for room in room-list)
	(iter (for i in *exits*)
	      (when (and (exit room i)
			 (not (find (dest-room (exit room i)) room-list)))
		(setf (exit room i) nil)))))

;; Инициализировать зону.
(defgeneric start-work (zone))

(defmethod start-work ((zone zone))
  (let ((mobs-type-count (length (mobs-spec zone))))
    (setf (mobs-counters zone) (make-array mobs-type-count :initial-element 0))
    (setf (mobs-max-numbers zone) (make-array mobs-type-count))
    (iter
      (for mob-spec in (mobs-spec zone))
      (for spawn-coords = (getf mob-spec :spawn-coords))
      (for spawn-limit = (getf mob-spec :spawn-limit 1))
      (for class = (case (getf mob-spec :type)
		     (:regular 'standard-mob)
		     (:banker 'banker)))
      (remove-from-plistf mob-spec :spawn-coords :spawn-limit :type)
      (for mob-index upfrom 0)
      (iter
        (repeat spawn-limit)
        (for (x y) = (random-elt spawn-coords))
        (assert (aref (map-array zone) y x))  ; TODO: check this when loading zone
	(for mob =  (make-mob-from-plist
                     class
		     (concatenate 'list mob-spec
				  (list :zone zone :mob-room (aref (map-array zone) x y)))))
	(push mob (mobs (aref (map-array zone) y x)))
	(schedule-mob-events mob (aref (map-array zone) y x) (zone-symbol zone)))
      (setf (aref (mobs-counters zone) mob-index) spawn-limit)
      (setf (aref (mobs-max-numbers zone) mob-index) spawn-limit))))

;; Временная функция для отладки. Добавляет в комнату с координатами (1, 1) собаку.
;; TODO:typo
(defmethod temp-start-work ((zone zone))
  ;; TODO: переместить этот триггер в данные банка или моба в файле зоны
  (push (list :bank-deposit-trigger nil
	      #'(lambda (room player sum)
		  (declare (ignore room))
		  (format (output player) "~a вложил ~a монет в банк.~%" (name player) sum)))
	(triggers (aref (map-array zone) 3 3)))
  #+nil
  (setf (triggers (aref (map-array zone) 1 1))
	(list
	 (list :player-left-room nil
	       #'(lambda (player room direction) (send-message-to-players (format nil "~A ушёл на ~A~%" (word-ip player) (direction-name direction))
									  (players room))))))
  (start-work zone))

;; Закончить работу зоны и все петли событий, чтобы освободить ресурсы.
;; Вызывать только когда все игроки вышли из зоны!
(defgeneric end-work (zone))

(defmethod end-work ((zone zone))
  (iter (with map = (map-array zone))
	(for y from 0 below (array-dimension map 1))
	(iter (for x from 0 below (array-dimension map 0))
	      (setf (mobs (aref map x y)) nil))))
