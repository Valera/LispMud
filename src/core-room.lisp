(in-package :lispmud)

(defvar *exits* '(:north :east :south :west) "All posible directions for iteration")

(defun reverse-direction (direction)
  "Returns reverse direction; for ex. :north is reverse for :south"
  (ecase direction
    (:south :north)
    (:north :south)
    (:west :east)
    (:east :west)))

(defun direction-name (direction)
  (case direction 
    (:east "восток")
    (:west "запад")
    (:south "юг")
    (:north "север")))

(defclass myroom ()
  ((short-description :accessor short-description :initform "" :initarg :short-description)
   (description :accessor description :initform "" :initarg :description)
   (mobs :accessor mobs :initform nil)
   (players :accessor players :initform nil)
   (items-on-floor :accessor items-on-floor :initform (list (make-instance 'item :name "мочалка") (make-instance 'item :name "кусок мыла")))
   (triggers :accessor triggers :initform nil)
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
  "Returns shift in X coordinate for moving in given direction."
  (ecase direction
    (:north 0) (:east  1) (:south 0) (:west  -1)))

(defun dy-for-direction (direction)
  "Returns shift in Y coordinate for moving in given direction."
  (ecase direction
    (:north -1) (:east  0) (:south 1) (:west  0)))

(defgeneric message-to-visitors (room message))
(defmethod message-to-visitors ((room myroom) message)
  (iter (for player in (players room))
	(message player message)))

(defgeneric room-about (room))
(defmethod room-about ((room myroom))
  (write-string *cc-blue*)
  (format t "В комнате ~a~%" (short-description *player-room*))
  (write-string *cc-reset*)
  (let ((items (items-on-floor room)))
    (if items
	(if (= (length items) 1)
	    (format t "На полу лежит ~a.~%" (name (first items)))
	    (format t "На полу лежат:~%~{  ~a~%~}" (mapcar #'name items)))))
  (when (mobs room)
      (format t "Мобы:~%~{  ~a~%~}" (mobs room)))
  (let ((other-players (remove *player* (players room))))
    (if other-players
	(format t "Игроки:~%~{  ~a~%~}" (mapcar #'name other-players))))
  (format t "~:[~;с~]~:[~;ю~]~:[~;з~]~:[~;в~]> "
	  (north-exit *player-room*)
	  (south-exit *player-room*)
	  (west-exit *player-room*)
	  (east-exit *player-room*))
  (force-output))

(defgeneric add-trigger (object trigger))

(defmethod add-trigger ((room myroom) trigger)
  (push trigger (triggers room)))

(defun process-room-triggers (room action-type &rest parameters)
  (labels ((call-if-matches (trigger)
	     (print 1)
	     (destructuring-bind (type trigger-fun action) trigger
	       (when (and (eql type action-type) (or (not trigger-fun) (apply trigger-fun parameters)))
		 (apply action parameters))))
	   (process-trigger-list (trigger-list)
	     (print 2)
	     (dolist (trig trigger-list)
	       (call-if-matches trig))))
    (process-trigger-list (triggers room))
#+(or)    (dolist (mob (mobs room))
      (process-trigger-list (triggers mob)))
#+(or)    (dolist (item (items-on-floor room))
      (process-trigger-list (triggers mob)))
#+(or)    (dolist (player (players room))
      (process-trigger-list (triggers player)))))
