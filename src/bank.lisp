;;; bank.lisp
    
(in-package :lispmud)

(defvar *bank* nil)
(defvar *bank-mutex* (make-mutex :name "bank mutex"))

(defun add-account (person &optional (start-balance 0))
  "Adds character account to bank."
  (with-mutex (*bank-mutex*)
    (push (list person start-balance) *bank*)))

(defun clear-bank ()
  "Clears bank totally, removing all accounts. Only "
  (with-mutex (*bank-mutex*)
    (setf *bank* nil)))

(defun deposit (person money)
  ""
  (with-recursive-lock (*bank-mutex*)
    (incf (second (find person *bank* :key #'first)) money)))

(defun withdraw (person money)
  (with-recursive-lock (*bank-mutex*)
  (let ((balance (second (find person *bank* :key #'first))))
    (if (> money balance)
	nil
	(decf (second (find person *bank* :key #'first)) money)))))

(defun balance (person)
  (with-recursive-lock (*bank-mutex*)
    (second (find person *bank* :key #'first))))

(defun transfer (person1 person2 sum)
  (with-recursive-lock (*bank-mutex*)
    (when (< sum (balance person1))
      (withdraw person1 sum)
      (deposit person2 sum))))
