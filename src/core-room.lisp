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
   (place-type :accessor place-type :initform nil :initarg :place-type)
   (mobless-p :accessor mobless-p :initform nil :initarg :mobless-p)
   (mobs :accessor mobs :initform nil)
   (players :accessor players :initform nil)
   (items-on-floor :accessor items-on-floor :initform
		   (list (make-instance 'coin-heap :coins (random 10)) (make-instance 'item :name "мочалка") (make-instance 'item :name "кусок мыла")))
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

(defgeneric (setf exit) (value room direction)
  (:method (value (room myroom) direction)
    (setf (slot-value room (exit-slot-for-direction direction)) value)))

(defun dx-for-direction (direction)
  "Returns shift in X coordinate for moving in given direction."
  (ecase direction
    (:north 0) (:east  1) (:south 0) (:west  -1)))

(defun dy-for-direction (direction)
  "Returns shift in Y coordinate for moving in given direction."
  (ecase direction
    (:north -1) (:east  0) (:south 1) (:west  0)))

(defun free-exits (room)
  (iter (for e in *exits*)
	(for exit = (exit room e))
	(when (and exit (can-pass exit))
	  (collect exit))))

(defgeneric message-to-visitors (room message))
(defmethod message-to-visitors ((room myroom) message)
  (iter (for p in (players room))
	(write-string message (output p))))

(defun message-to-visitors-except (player room message)
  (iter (for p in (remove player (players room)))
	(write-string message (output p))))

(defgeneric room-about (room))
(defmethod room-about ((room myroom))
  (write-string *cc-cyan*)
  (format t "~a~%" (short-description *player-room*))
  (write-string *cc-reset*)
  (write-string (description *player-room*))
  (write-char #\Newline)
  (when (mobs room)
    (format t "Мобы:~%")
    (write-string *cc-red*)
    (format t "~{  ~a~%~}"
	    (mapcar #'(lambda (mob) (string-capitalize (name mob) :end 1)) (mobs room)))
    (write-string *cc-reset*))
  (let ((items (items-on-floor room)))
    (if items
	(if (= (length items) 1)
	    (format t "На полу лежит ~a.~%" (name (first items)))
	    (format t "На полу лежат:~%~{  ~a~%~}" (mapcar #'name items)))))
  (let ((other-players (remove *player* (players room))))
    (if other-players
	(format t "Игроки:~%~{  ~a~%~}" (mapcar #'name other-players))))
  (format t "~:[~;С~]~:[~;Ю~]~:[~;З~]~:[~;В~] ~a$ > "
	  (north-exit *player-room*)
	  (south-exit *player-room*)
	  (west-exit *player-room*)
	  (east-exit *player-room*)
	  (money *player*))
  (force-output))

(defgeneric add-trigger (object trigger))
(defmethod add-trigger ((room myroom) trigger)
  (push trigger (triggers room)))

(defun process-room-triggers (room action-type &rest parameters)
  (labels ((call-if-matches (trigger)
	     (destructuring-bind (type trigger-fun action) trigger
	       (when (and (eql type action-type) (or (not trigger-fun) (apply trigger-fun parameters)))
		 (apply action room parameters))))
	   (process-trigger-list (trigger-list)
	     (dolist (trig trigger-list)
	       (call-if-matches trig))))
    (process-trigger-list (triggers room))
#+(or)    (dolist (mob (mobs room))
      (process-trigger-list (triggers mob)))
#+(or)    (dolist (item (items-on-floor room))
      (process-trigger-list (triggers mob)))
#+(or)    (dolist (player (players room))
      (process-trigger-list (triggers player)))))

(defclass service-room (myroom)
  ((service-mob :accessor service-mob :initarg :service-mob)))

(defclass shop-room (service-room)
  ((price-list :accessor price-list :initarg :price-list)))

(defclass bank-room (service-room)
  ())

(defclass hotel-room (service-room)
  ())



