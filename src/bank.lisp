;;; bank.lisp
    
(in-package :lispmud)

(defvar *bank* (make-hash-table :test 'equal))

(defun add-account (person &optional (start-balance 0))
  "Adds character account to bank."
  (setf (gethash person *bank*) start-balance))

(defun clear-bank ()
  "Clears bank totally, removing all accounts. Only "
  (setf *bank* (make-hash-table :test 'equal)))

(defun deposit (person sum)
  "Deposit given sum of money to person's accont."
  (setf (gethash person *bank*) (+ sum (gethash person *bank* 0))))
 
(defun withdraw (person sum)
  "Withdraw given sum of money from person's accont."
  (let ((balance (gethash person *bank*)))
    (if (> sum balance)
	nil
	(decf (gethash person *bank*) sum))))

(defun balance (person)
  (gethash person *bank*))

(defun transfer (person1 person2 sum)
  (when (< sum (balance person1))
    (withdraw person1 sum)
    (deposit person2 sum)))
