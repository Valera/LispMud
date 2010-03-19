(in-package :lispmud)

(defun shuffle-list (list)
  (iter (with l = (length list))
	(for i from 0 below l)
	(rotatef (elt list i) (elt list (random l))))
  list)

(defclass mob ()
  ((name :accessor name :initarg :name)
   (description :accessor description :initarg :description)
   behaviour
   items
   (zone :accessor zone :initarg :zone)))

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
  (queue-mesg (room mob) :leave (format nil "~a вышел из комнаты" (name mob))))

(defgeneric enter (mob room))
(defmethod enter ((mob mob) (room myroom))
  (push mob (mobs room))
  ;(setf (room mob) room)
  (queue-mesg (room mob) :enter (format nil "~a вышел из комнаты" (name mob))))

;; Сделать что-нибудь, чтобы казаться настоящим.
(defgeneric activity (mob))
(defmethod activity :after ((mob mob))
  (queue-mesg (zone mob) :mob-activity (list 'activity) :timeout 20))

;; (defmethod initialize-instance
(defmethod initialize-instance :after ((mob mob) &rest initargs)
  (declare (ignore initargs))
  (queue-mesg (zone mob) :mob-activity (list 'activity) :timeout 20))

(defclass dog (mob) ())

(defmethod initialize-instance :after ((dog dog) &rest initargs)
  (declare (ignore initargs))
  (setf (name dog) "Cобака"))
