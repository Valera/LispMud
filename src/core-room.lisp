(in-package :lispmud)

(defvar *exits* '(:north :east :south :west) "All posible directions for iteration")

(defun reverse-direction (direction)
  "Returns reverse direction; for ex. :north is reverse for :south"
  (ecase direction
    (:south :north)
    (:north :south)
    (:west :east)
    (:east :west)))

(defclass myroom ()
  ((short-description :accessor short-description :initform "" :initarg :short-description)
   (description :accessor description :initform "" :initarg :description)
   (mobs :accessor mobs :initform nil)
   (players :accessor players :initform nil)
   (west-exit :accessor west-exit :initform nil :initarg :west-exit)
   (east-exit :accessor east-exit :initform nil :initarg :east-exit)
   (south-exit :accessor south-exit :initform nil :initarg :south-exit)
   (north-exit :accessor north-exit :initform nil :initarg :north-exit)))

(defclass exit ()
  ((dest-room :accessor dest-room :initarg :dest-room)))

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
  
(defgeneric move-in-direction (room direction))
(defgeneric exit (room direction))

(defun exit-slot-for-direction (direction)
  "For given direction keyword, returns name of matching exit slot in class room."
  (ecase direction
    (:north 'north-exit)
    (:east  'east-exit)
    (:south 'south-exit)
    (:west  'west-exit)))

(defmethod exit ((room myroom) direction)
  "Returns value of exit slot for given direction."
  (slot-value room (exit-slot-for-direction direction)))

(defun dx-for-direction (direction)
  "Returns shift in X coordinate for moving in direction parameter."
  (ecase direction
    (:north 0) (:east  1) (:south 0) (:west  -1)))

(defun dy-for-direction (direction)
  (ecase direction
    (:north -1) (:east  0) (:south 1) (:west  0)))

(defgeneric message-to-visitors (room message))
(defmethod message-to-visitors ((room myroom) message)
  (iter (for player in (players room))
	(message player message)))

(defgeneric room-about (room))
(defmethod room-about ((room myroom))
  (format t "В комнате ~a~%" (short-description *player-room*))
  (if (mobs room)
      (format t "Мобы:~%~{  ~a~%~}" (mobs room))))
