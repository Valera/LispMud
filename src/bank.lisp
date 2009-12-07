;;; bank.lisp
    
(in-package :lispmud)

(defvar *bank* (make-hash-table :test 'equal))
(defvar *bank-mutex* (make-mutex :name "bank mutex"))

(defun add-account (person &optional (start-balance 0))
  "Adds character account to bank."
  (with-mutex (*bank-mutex*)
    (setf (gethash person *bank*) start-balance)))

(defun clear-bank ()
  "Clears bank totally, removing all accounts. Only "
  (with-mutex (*bank-mutex*)
    (setf *bank* (make-hash-table :test 'equal))))

(defun deposit (person sum)
  "Deposit given sum of money to person's accont."
  (with-recursive-lock (*bank-mutex*)
    (incf (gethash person *bank*) sum)))
 
(defun withdraw (person sum)
  "Withdraw given sum of money from person's accont."
  (with-recursive-lock (*bank-mutex*)
    (let ((balance (gethash person *bank*)))
      (if (> sum balance)
	  nil
	  (decf (gethash person *bank*) sum)))))

(defun balance (person)
  (with-recursive-lock (*bank-mutex*)
    (gethash person *bank*)))

(defun transfer (person1 person2 sum)
  (with-recursive-lock (*bank-mutex*)
    (when (< sum (balance person1))
      (withdraw person1 sum)
      (deposit person2 sum))))
