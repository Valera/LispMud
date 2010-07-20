;;; main.lisp

(in-package :lispmud)

(defvar *mail-queue* () )
(defvar *mail-queue-lock* (make-lock "Mail system lock"))

(defun send-mail (from-hero to-hero message)
  (bt:with-recursive-lock-held (*mail-queue-lock*)
    (push (list from-hero to-hero message) *mail-queue*)))

(defun recv-mail (receiver-hero)
  (bt:with-recursive-lock-held (*mail-queue-lock*)
    (prog1
	(remove-if-not #'(lambda (recv) (string= recv receiver-hero))
		       *mail-queue* :key #'second)
      (delete-if #'(lambda (recv) (string= recv receiver-hero)) *mail-queue*)))

(defun check-mail (receiver-name)
  (bt:with-recursive-lock-held (*mail-queue-lock*)
    (not (null (position receiver-name *mail-queue* :key #'second)))))
