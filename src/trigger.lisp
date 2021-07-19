;;; trigger.lisp

(in-package :cl-user)
(defpackage :lispmud/trigger
  (:use :cl)
  (:import-from :lispmud/player #:output))
(in-package :lispmud/trigger)

#+(or)
(defvar *registered-triggers* nil
  "Список зарегестрированных тригеров.")

#+(or)
(defclass action-table ()
  ((actions :accessor actions :initform nil)))

#+(or)
(defgeneric add-action (action-table type trigger action)
  (:method ((table action-table) type trigger action)
    (push (list type trigger action) (actions table))))

#+(or)
(defun react (action-table action-type &rest parameters)
  (iter (for (type trigger action) in (actions action-table))
	(when (and (eql type action-type) trigger (apply trigger parameters))
	  (funcall action parameters))))

(defun send-message-to-players (message players)
  (dolist (p players)
    (write-string message (output p))))

;--
;player-enter
;#'(lambda (room) (players room))
;#'(lambda (room entered-player) (send-message-to-players
;				 (format nil "В комнату вошёл ~a~%" (word-ip entered-player))
;				 (players room))
;--
;player-exit
;#'(lambda (room) (players room))
;#'(lambda (room ...))
;--

;trigger types:
;player-entered
;player-left
;player-took
;player-gave
;player-killed

;monster-killed
;monster-took

;item-about-to-be-taken
