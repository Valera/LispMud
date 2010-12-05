;;; main.lisp

(in-package :lispmud)

(defvar *mail-queue* () )
(defvar *mail-queue-lock* (make-lock "Mail system lock"))

(defun send-mail (sender-name receiver-name message)
  (bt:with-recursive-lock-held (*mail-queue-lock*)
    (push (list sender-name receiver-name message) *mail-queue*)))

(defun recv-mail (receiver-hero)
  (bt:with-recursive-lock-held (*mail-queue-lock*)
    (prog1
	(remove-if-not #'(lambda (recv) (string= recv receiver-hero))
		       *mail-queue* :key #'second)
      (setf *mail-queue*
	    (delete-if #'(lambda (message) (string= (second message) receiver-hero)) *mail-queue*)))))

(defun have-mail-for (receiver-hero)
  (bt:with-lock-held (*mail-queue-lock*)
    (position (name receiver-hero) *mail-queue* :key #'second :test #'string=)))

