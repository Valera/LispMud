;;; main.lisp

(in-package :lispmud)

(defvar *mail-queue* () )
(defvar *mail-queue-mutex* (make-mutex :name "Mail system mutex"))

(defun send-mail (from-hero to-hero message)
  (with-recursive-lock (*mail-queue-mutex*)
    (push (list from-hero to-hero message) *mail-queue*)))

(defun recv-mail (receiver-hero)
  (with-recursive-lock (*mail-queue-mutex*)
    (remove-if-not #'(lambda (recv) (string= recv receiver-hero))
		   *mail-queue* :key #'second)
    (delete-if #'(lambda (recv) (string= recv receiver-hero)) *mail-queue*)))

(defun check-mail (receiver-name)
  (with-recursive-lock (*mail-queue-mutex*)
    (not (null (position receiver-name *mail-queue* :key #'second)))))
