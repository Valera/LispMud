;;; main.lisp

(in-package :cl-user)
(defpackage :lispmud/core-mail
  (:use :cl)
  (:import-from :lispmud/core-utils #:name))
(in-package :lispmud/core-mail)

(defun send-mail (sender-name receiver-name message-text)
  (pomo:execute (:insert-into 'letters
			      :set
			      'sender-name sender-name
			      'receiver-name receiver-name
			      'message-text message-text)))

(defun recv-mail (receiver-hero)
  (pomo:query (:select
	       'sender-name 'receiver-name 'message-text
	       :from 'letters
	       :where (:= 'receiver-name (name receiver-hero)))))

(defun have-mail-for (receiver-hero)
  (pomo:query (:select
	       'receiver-name
	       :from 'letters
	       :where (:= 'receiver-name (name receiver-hero)))))
