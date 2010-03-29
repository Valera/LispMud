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
   (message-queue :accessor message-queue :initform nil)))

(defmethod initialize-instance :after ((zone zone) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value zone 'id) *zone-id-source*)
  (incf *zone-id-source*)
  (setf (lock zone) (make-lock (concatenate 'string "Zone mutex: " (name zone)))))

(defgeneric queue-mesg (zone mesg-type arguments &key timeout))
(defmethod queue-mesg ((zone zone) mesg-type arguments &key timeout)
  (if timeout
      (schedule-timer (make-timer #'(lambda ()
				      (queue-mesg zone mesg-type arguments)))
		      2)
      (enqueue (list mesg-type arguments) (message-queue zone))))

(defgeneric get-mesg (zone))
(defmethod get-mesg ((zone zone))
  (dequeue (message-queue zone)))

(defgeneric event-loop (zone))
(defmethod event-loop ((zone zone))
  (iter
    (repeat 200)
    (queue-mesg zone :mob-activity ()))
  (iter
    (for message = (get-mesg zone))
    (case message
      (:mob-activity ()))))

(defun load-zone (filename)
  (with-open-file (stream filename)
    (destructuring-bind
	  (&key zone-name zone-rooms zone-mobs
		((:zone-size (size-x size-y))))
	(read stream)
      (pvalue zone-name zone-rooms zone-mobs
	      (list size-x size-y))
      (let ((map (make-array (list size-x size-y) :initial-element nil))
	    (entry-rooms nil))
	(dolist (room zone-rooms)
	  (pvalue room)
	  (destructuring-bind
		(&key ((:coord (x y)))
		      room-short-description room-long-description room-flags room-type
		      west-exit east-exit north-exit south-exit)
	      room
	    (setf (aref map y x)
		  (make-instance 'myroom :short-description room-short-description
				 :description room-long-description
				 :west-exit west-exit :east-exit east-exit
				 :north-exit north-exit :south-exit south-exit))
	    (push (aref map y x) entry-rooms)))
	(link-rooms map)
	(make-instance 'zone :name zone-name :map-array map :entry-rooms entry-rooms)  ))))

(defun link-rooms (zone-map)
  (pvalue "link-rooms" zone-map)
  (iter
    (with size-x = (array-dimension zone-map 0))
    (with size-y = (array-dimension zone-map 1))
    (for (y x room) in-matrix zone-map)
    (if room
	(dolist (direction *exits*)
	  (let (x1 y1 room1)
	    (if (exit room direction)
		(if (or (>= (setf x1 (+ (dx-for-direction direction) x)) size-x)
			(> 0 x1); x)
			(>= (setf y1 (+ (dy-for-direction direction) y)) size-y)
			(> 0 y1); y)
			(not (setf room1 (aref zone-map y1 x1)))
			(not (exit room1 (reverse-direction direction))))
		    (progn
		      (pvalue x y x1 y1 room1)
		      (format t "Warning: ~a exit in room (~a, ~a) doen't have pair~%"
			      direction x y))
		    (progn
					;(pvalue x y x1 y1 room1)
		      (setf (slot-value room (exit-slot-for-direction direction))
			    (make-instance 'exit :dest-room room1))))))))))

;; Инициализировать зону.
(defgeneric start-work (zone))

(defmethod start-work ((zone zone))
  (let ((mobs-type-count (length (mobs-spec zone))))
    (setf (mobs-counters zone) (make-array mobs-type-count :initial-element 0))
    (setf (mobs-max-numbers zone) (make-array mobs-type-count))
    (iter (for (mob-class x y respawn-rate max-number) in (mobs-spec zone))
	  (for i upfrom 0)
	  (push (make-instance mob-class :zone zone) (mobs (aref (map-array zone) x Y)))
	  (incf (aref (mobs-counters zone) i))
	  (setf (aref (mobs-max-numbers zone) i) max-number))))

(defmethod temp-start-work ((zone zone))
  (setf (mobs-spec zone) (list (list 'dog 1 1 300 1)))
  (start-work zone))

;; Закончить работу зоны и все петли событий, чтобы освободить ресурсы.
;; Вызывать только когда все игроки вышли из зоны!
(defgeneric end-work (zone))

(defmethod end-work ((zone zone))
  (iter (with map = (map-array zone))
	(for y from 0 below (array-dimension map 1))
	(iter (for x from 0 below (array-dimension map 0))
	      (setf (mobs (aref map x y)) nil))))
