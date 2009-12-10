(in-package :lispmud)

(defclass room1 ()
  ((south :initarg :south)
   (north :initarg :north)
   (west  :initarg :west)
   (east  :initarg :east)
   (description :initarg :description :accessor description)))

(defun reverse-direction (direction)
  "Returns reverse direction; for ex. :north is reverse for :south"
  (ecase direction
    (south 'north)
    (north 'south)
    (west 'east)
    (east 'west)))

(defclass room ()
  ())

(defclass zone ()
  ((room-list)))

(defgeneric move-in-direction (room direction))

(defun load-zone (filename)
  (with-open-file (stream filename)
    (destructuring-bind
	  (&key zone-description zone-rooms zone-mobs
		((:zone-size (size-x size-y))))
	(read stream)
      (pvalue zone-description zone-rooms zone-mobs
	      (list size-x size-y))
      (make-array `(,size-x ,size-y)))))
