;;; core-zone.lisp

(in-package :lispmud)

(defvar *zone-id-source* 0
  "Источник идентификаторов для зон. После создания каждой зоны увеличивается на 1.")

(defclass zone ()
  ((name :accessor name :initarg :name)
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

(defun save-zone (zone filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((map-array (map-array zone)))
      (pprint
       (list :zone-name (name zone)    :mobs-spec (mobs-spec zone)
	     :zone-size (array-dimensions map-array)
	     :zone-rooms
	     (iter (for y from 0 below (array-dimension map-array 0))
		   (nconcing
		    (iter (for x from 0 below (array-dimension map-array 1))
			  (for r = (aref map-array y x))
			  (when r
			    (with-accessors ((west-exit west-exit) (east-exit east-exit)
					     (south-exit south-exit) (north-exit north-exit))
				r ;(west-exit east-exit north-exit south-exit) r
			      (flet ((exit-sexpr (room-on-exit)
				       (when room-on-exit
					 (list :exit-description (short-description (dest-room room-on-exit)) :door nil))))
				(collect (list :room-class (or (getf (editor-info r) :room-class)
							       (class-name (class-of r)))
					       :room-short-description (short-description r)
					       :room-long-description (description r)
					       :room-flags ()
					       :room-type (place-type r)
					       :coord (list x y)
					       :west-exit (exit-sexpr west-exit)
					       :east-exit (exit-sexpr east-exit)
					       :north-exit (exit-sexpr north-exit)
					       :south-exit (exit-sexpr south-exit))))))))))
       stream))))

(defun load-zone (filename)
  (with-open-file (stream filename)
    (destructuring-bind (&key zone-name zone-rooms mobs-spec
			      ((:zone-size (size-x size-y))))
	(let ((*package* (find-package :lispmud)))
	  (read stream))
      (pvalue zone-name zone-rooms (list size-x size-y) mobs-spec)
      (let ((map (make-array (list size-x size-y) :initial-element nil))
	    (entry-rooms nil))
	(dolist (room zone-rooms)
	  (pvalue room)
	  (destructuring-bind
		(&key ((:coord (x y)))
		      room-class room-short-description room-long-description room-flags room-type
		      west-exit east-exit north-exit south-exit)
	      room
	    (assert (subtypep room-class 'myroom))
	    (setf (aref map y x)
		  (make-instance room-class :short-description room-short-description
				 :description room-long-description
				 :place-type room-type :flags room-flags
				 :west-exit west-exit :east-exit east-exit
				 :north-exit north-exit :south-exit south-exit))
	    (push (aref map y x) entry-rooms)))
	(link-rooms map)
	(make-instance 'zone :name zone-name :map-array map
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
    (iter (for mob-spec in (mobs-spec zone))
	  (for x = 1)
	  (for y = 1)
	  (for class = (let ((type (getf mob-spec :type)))
			 (case type 
			   (:regular 'standard-mob)
			   (:banker 'banker))))
	  (remf mob-spec :type)
	  (for i upfrom 0)
	  (for mob =  (make-mob-from-plist class
					   (concatenate 'list mob-spec
							(list :zone zone :mob-room (aref (map-array zone) x y)))))
	  (push mob (mobs (aref (map-array zone) x y)))
	  (schedule-mob-events mob (aref (map-array zone) x y) zone)
	  (incf (aref (mobs-counters zone) i))
#+nil	  (setf (aref (mobs-max-numbers zone) i) max-number))))

;; Временная функция для отладки. Добавляет в комнату с координатами (1, 1) собаку.
;; TODO:typo
(defmethod temp-start-work ((zone zone))
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
