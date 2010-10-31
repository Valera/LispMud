;;; mob.lisp

(in-package :lispmud)

(defclass mob ()
  ((name :accessor name :initarg :name)
   (description :accessor description :initarg :description)
   (hp :accessor hp)
   (max-hp :accessor max-hp :initarg :map-hp :initform 10)
   (mob-room :accessor mob-room :initarg :mob-room)
   behaviour
   items
   (zone :accessor zone :initarg :zone)))

(defmethod initialize-instance :after ((mob mob) &rest initargs)
  (declare (ignore initargs))
  (setf (hp mob) (max-hp mob)))

(defgeneric battle (mob mob2-or-player))

(defgeneric move (mob))
(defmethod move ((mob mob))
  (let* ((room (room mob))
	 (neighbours (remove-if #'(lambda (r) (mobless-p r))
				 (neighbour-rooms room)))
	 (next-room (elt neighbours (random (length neighbours)))))
    (when next-room
      (go-out mob (room mob))
      (enter mob next-room))))

(defgeneric go-out (mob room))
(defmethod go-out ((mob mob) (room myroom))
  (removef mob (mobs room))
  ;(setf (room mob) nil)
#+nil  (queue-mesg (room mob) :leave (format nil "~a вышел из комнаты" (name mob))))

(defgeneric enter (mob room))
(defmethod enter ((mob mob) (room myroom))
  (push mob (mobs room))
  ;(setf (room mob) room)
#+nil  (queue-mesg (room mob) :enter (format nil "~a вышел из комнаты" (name mob))))

;; Сделать что-нибудь, чтобы казаться настоящим.
(defgeneric idle-animation (mob))


;; Собака
(defclass dog (mob) ()
  (:default-initargs :name "собака"))

(defmethod initialize-instance :after ((dog dog) &rest initargs)
  (declare (ignore initargs))
  (add-event 3 #'(lambda () (idle-animation dog)) nil 30))

(defmethod idle-animation ((mob dog))
  (iter (for p in (players (mob-room mob)))
	(format (output p) "~&Собака почесала себя лапой за ухом и зевнула.~%")))

;; Гном из банка.
(defclass bank-gnome (mob) ()
  (:default-initargs :name "седой гном"))

(defmethod initialize-instance :after ((gnome bank-gnome) &rest initargs)
  (declare (ignore initargs))
  (add-event 3 #'(lambda () (idle-animation gnome)) nil 40))

(defmethod idle-animation ((mob bank-gnome))
  (iter (for p in (players (mob-room mob)))
	(format (output p) "~&Седой гном перелистнул страницу в банковской книге и щёлкнул костяшками на счётах.~%")))

