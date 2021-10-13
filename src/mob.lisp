;;; mob.lisp

(in-package :cl-user)
(defpackage :lispmud/mob
  (:use :cl)
  (:use :iter)
  (:use :lispmud/rucase)
  (:import-from :alexandria #:deletef #:emptyp)
  (:import-from :lispmud/core-utils #:name)
  (:import-from :lispmud/event-timer #:add-event)
  (:import-from :lispmud/player #:output #:inventory)
  (:import-from :lispmud/core-threadvars #:*player*)
  (:import-from :lispmud/core-mail #:recv-mail #:have-mail-for)
  (:import-from :lispmud/core-items #:letter)
  (:import-from :lispmud/core-room #:description #:short-description
                #:dest-room #:east-exit #:west-exit #:north-exit #:south-exit
                #:editor-info #:place-type #:myroom #:reverse-direction
                #:dx-for-direction #:dy-for-direction #:*exits* #:direction
                #:exit #:exit-slot-for-direction #:mobs #:triggers #:can-pass
                #:mobless-p #:message-to-visitors #:message-to-visitors-except
                #:direction-name))
(in-package :lispmud/mob)

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

(defun open-rooms-and-directions (room)
  (iter (for e in *exits*)
	(for exit = (exit room e))
	(when (and exit (can-pass exit) (not (mobless-p (dest-room exit))))
	  (collect (list (dest-room exit) e)))))

(defgeneric move (mob))
(defmethod move ((mob mob))
  (let* ((neighbour-rooms-and-directions (open-rooms-and-directions (mob-room mob)))
	 (next-room-and-direction (elt neighbour-rooms-and-directions (random (length neighbour-rooms-and-directions))))
	 (next-room (first next-room-and-direction))
	 (direction (second next-room-and-direction)))
    (when next-room
      (go-out mob (mob-room mob) direction)
      (enter mob next-room direction))))

(defgeneric go-out (mob room direction))
(defmethod go-out ((mob mob) (room myroom) direction)
  (deletef (mobs room) mob))

(defgeneric enter (mob room direction))
(defmethod enter ((mob mob) (room myroom) direction)
  (setf (mob-room mob) room)
  (push mob (mobs room)))

(defclass mail-dragon (mob) ()
  (:default-initargs :name "Синий дракончик"))

(defun deliver-mail-for (player room)
  (unless (have-mail-for player)
    (format (output player) "Вам не пришло писем.")
    (return-from deliver-mail-for))
  (make-instance 'mail-dragon :mob-room room) ; TODO: use this mob as actual mob
  (message-to-visitors room (format nil "~&Внезапно, как из ниоткуда, в воздухе появляется синий дракончик.~%"))
  (message-to-visitors-except player room (format nil "~&Дракончик с интересом огладывается по сторонами и подлетает к ~a~%" (word-dp player)))
  (format (output player) "~&Дракончик с интересом огладывается по сторонами и подлетает к вам.~%")
  (let ((messages (recv-mail *player*)))
    (apply 'format (output player) "~&Синий дракончик вытаскивает из сумки на шее ~A и отдаёт ~A вам.~%"
	   (if (> (length messages) 1)
	       (list (format nil "~a письма" (length messages)) "их")
	       (list (format nil "письмо") "его")))
    (iter (for m in messages)
      (push (make-instance 'letter :text (third m)) (inventory *player*)))))

;; STANDARD-MOB

(defclass standard-mob (mob)
  ((enter-verb :initarg :enter-verb :reader enter-verb)
   (leave-verb :initarg :leave-verb :reader leave-verb)
   (move-interval :initarg :move-interval :reader move-interval)
   (animation-1 :initarg :animation-1 :reader animation-1)
   (animation-1-timer :initarg :animation-1-timer :reader animation-1-timer)
   (animation-2 :initarg :animation-2 :reader animation-2)
   (animation-2-timer :initarg :animation-2-timer :reader animation-2-timer))
  (:documentation "Класс для удобства автоматического создания мобов"))

(defun make-mob-from-plist (class plist)
  (apply 'make-instance class plist))

(defun plist-from-mob (mob)
  (with-accessors ((n name) (d description)) mob
    (list
     :name n
     :description d)))

(defgeneric schedule-mob-events (mob room zone))
(defgeneric move-to-other-room (mob))
(defgeneric leave-message (mob room-from room-to direction))
(defgeneric enter-message (mob room-from room-to direction))
(defgeneric do-animation1 (mob room))
(defgeneric do-animation2 (mob room))

(defmethod schedule-mob-events ((mob standard-mob) room zone-symbol)
  (bind:bind (((:slots animation-1 animation-1-timer
		       animation-2 animation-2-timer
		       move-interval)
	       mob)
	      ((:flet schedule-animation (animation animation-timer))
	       (when (and animation (not (emptyp animation))
			  (/= 0 animation-timer))
		 (print (list 1 animation animation-timer))
		 (add-event animation-timer
			    #'(lambda () (message-to-visitors (mob-room mob) animation))
			    zone-symbol animation-timer))))
    (schedule-animation animation-1 animation-1-timer)
    (schedule-animation animation-2 animation-2-timer)
    (print (list 3 move-interval))
    (unless (= 0 move-interval)
      (add-event move-interval #'(lambda () (move-to-other-room mob))
		 zone-symbol move-interval))))

(defmethod move-to-other-room ((mob standard-mob))
  (let* ((neighbour-rooms-and-directions (open-rooms-and-directions (mob-room mob)))
	 (next-room-and-direction (elt neighbour-rooms-and-directions (random (length neighbour-rooms-and-directions))))
	 (next-room (first next-room-and-direction))
	 (direction (second next-room-and-direction)))
    (when next-room
      (leave-message mob (mob-room mob) next-room direction)
      (go-out mob (mob-room mob) direction)
      (enter mob next-room direction)
      (enter-message mob (mob-room mob) next-room direction))))

(defmethod leave-message ((mob standard-mob) room-from room-to direction)
  (message-to-visitors
   room-from
   (format nil "~A ~A на ~A.~%"
	   (name mob) (leave-verb mob) (direction-name direction))))

(defmethod enter-message ((mob standard-mob) room-from room-to direction)
  (message-to-visitors
   room-to
   (format nil "~A ~A с ~A.~%"
	   (name mob) (enter-verb mob)
	   (word-rp (direction-name (reverse-direction direction))))))

;; BANK-MOB

(defclass banker (standard-mob)
  ((withdraw-phrase :initarg :withdraw-phrase :reader withdraw-phrase)
   (deposit-phrase :initarg :deposit-phrase :reader deposit-phrase)))

