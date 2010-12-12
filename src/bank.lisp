;;; bank.lisp
    
(in-package :lispmud)

(defun deposit (person sum)
  "Deposit given sum of money to person's accont."
  (assert (plusp sum))
  (pomo:execute (:update 'players
			 :set 'money-in-bank (:+ sum 'money-in-bank)
			 :where (:= 'name person))))
(defun balance (person)
  (pomo:query (:select
	       'money-in-bank
	       :from 'players
	       :where (:= 'name person))
	      :single! t))

(defun withdraw (person sum)
  "Withdraw given sum of money from person's accont."
  (assert (plusp sum))
  (pomo:with-transaction ()
    (let ((balance (balance person)))
      (if (> sum balance)
	  nil
	  (pomo:execute (:update
			 'players
			 :set
			 'money-in-bank (:- 'money-in-bank sum)))))))

(defun transfer (person1 person2 sum)
  (pomo:with-transaction ()
    (when (< sum (balance person1))
      (withdraw person1 sum)
      (deposit person2 sum))))
