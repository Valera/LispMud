(in-package :lispmud)

(defclass room1 ()
  ((south :initarg :south)
   (north :initarg :north)
   (west  :initarg :west)
   (east  :initarg :east)
   (description :initarg :description)))

(defun reverse-direction (direction)
  "Returns reverse direction; for ex. :north is reverse for :south"
  (ecase direction
    (south 'north)
    (north 'south)
    (west 'east)
    (east 'west)))
