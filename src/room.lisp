(in-package :lispmud)

;; FIXME: delete class room1.
(defclass room1 ()
  ((south :initarg :south)
   (north :initarg :north)
   (west  :initarg :west)
   (east  :initarg :east)
   (description :initarg :description :accessor description)))

(defvar *exits* '(:north :east :south :west) "All posible directions for iteration")

(defun reverse-direction (direction)
  "Returns reverse direction; for ex. :north is reverse for :south"
  (ecase direction
    (south 'north)
    (north 'south)
    (west 'east)
    (east 'west)))

(defclass myroom ()
  ((short-description :accessor short-description :initform "" :initarg :short-description)
   (description :accessor description :initform "" :initarg :description)
   (west-exit :accessor west-exit :initform nil :initarg :west-exit)
   (east-exit :accessor east-exit :initform nil :initarg :east-exit)
   (south-exit :accessor south-exit :initform nil :initarg :south-exit)
   (north-exit :accessor north-exit :initform nil :initarg :north-exit)))

(defclass exit ()
  ((dest-room :accessor dest-room :initarg :accessor)))

(defgeneric can-pass (exit))

(defmethod can-pass ((exit exit))
  t)

(defclass passage (exit)
  ()
  (:documentation "простой проход"))

(defclass door (exit)
  ((opened-p :accessor opened-p :initarg opened-p :initform t)
   (unlock-key :accessor unlock-key :initarg unlock-key :initform nil))
  (:documentation "Дрерь может быть закрыта и открыта"))

(defclass hotel (exit)
  ()
  (:documentation "персонаж отдыхает в гостиннице, пока игрок ушёл из игры"))

(defclass portal (exit)
  ()
  (:documentation "проход в другую зону"))

(defclass zone ()
  ((map-array :accessor map-array :initarg :map-array)
   (entry-rooms :accessor entry-rooms :initarg :entry-rooms)))

(defgeneric move-in-direction (room direction))
(defgeneric exit (room direction))

(defun exit-slot-for-direction (direction)
  (ecase direction
    (:north 'north-exit)
    (:east  'east-exit)
    (:south 'south-exit)
    (:west  'west-exit)))

(defmethod exit ((room myroom) direction)
  (slot-value room (exit-slot-for-direction direction)))

(defun dx-for-direction (direction)
  (ecase direction
    (:north 0) (:east  1) (:south 0) (:west  -1)))

(defun dy-for-direction (direction)
  (ecase direction
    (:north 1) (:east  0) (:south -1) (:west  0)))

(defun load-zone2 (filename)
  (with-open-file (stream filename)
    (destructuring-bind
	  (&key zone-description zone-rooms zone-mobs
		((:zone-size (size-x size-y))))
	(read stream)
      (pvalue zone-description zone-rooms zone-mobs
	      (list size-x size-y))
      (let ((map (make-array (list size-x size-y) :initial-element nil))
	    (entry-rooms nil))
	(dolist (room zone-rooms)
	  (pvalue room)
	  (destructuring-bind
		(&key ((:coord (x y)))
		      room-description
		      west-exit east-exit north-exit south-exit)
	      room
	    (setf (aref map x y)
		  (make-instance 'myroom :description room-description
				 :west-exit west-exit :east-exit east-exit
				 :north-exit north-exit :south-exit south-exit))
	    (push (aref map x y) entry-rooms)))
	(link-rooms map)
	(make-instance 'zone :map-array map :entry-rooms entry-rooms)  ))))

(defun link-rooms (zone-map)
  (let ((size-x (array-dimension zone-map 0))
	(size-y (array-dimension zone-map 1)))
    (dotimes (x size-x)
      (dotimes (y size-y)
	(let ((room (aref zone-map x y)))
	  (if room
	      (dolist (direction *exits*)
		(let (x1 y1 room1)
		  (if (exit room direction)
		      (if (or (>= (setf x1 (+ (dx-for-direction direction) x)) size-x)
			      (< 0 x1 x)
			      (>= (setf y1 (+ (dy-for-direction direction) y)) size-y)
			      (< 0 y1 y)
			      (not (setf room1 (aref zone-map x1 y1)))
			      (not (exit room1 (reverse-direction direction))))
			  (format t "Warning: ~a exit in room (~a, ~a) doen't have pair"
			      direction x y)
			  (setf (slot-value room (exit-slot-for-direction direction))
					    (make-instance 'exit :dest-room room1))))))))))))
