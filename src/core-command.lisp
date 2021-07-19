;;; command.lisp

(in-package :cl-user)
(defpackage :lispmud/core-command
  (:use :cl)
  (:import-from :alexandria #:hash-table-keys #:set-equal)
  (:import-from :iter #:iter #:for #:collecting)
  (:import-from :split-sequence #:split-sequence))
(in-package :lispmud/core-command)

(defvar *command-hash* (make-hash-table :test 'equal)
  "Хэш-таблица, ставит в соответсвие строке команды функцию для её обработки")

(defun init-command-table (commands-spec)
  (let 	((command-hash (make-hash-table :test 'equal)))
    (iter (for (command-name command-fun) in commands-spec)
	  (dotimes (j (length command-name ))
	    (let ((substr (subseq command-name  0 (1+ j))))
	      (unless (gethash substr command-hash)
		(setf (gethash substr command-hash) command-fun)))))
    (setf *command-hash* command-hash)))

(defun exec-command (command-string)
  (let ((command-and-args (split-sequence #\Space command-string  :remove-empty-subseqs t)))
    (if command-and-args
	(destructuring-bind (command &rest args) command-and-args
	  (let ((command-fun (gethash command *command-hash*)))
	    (if command-fun
		(apply command-fun args)
		(format t "Комманда не найдена.~%"))))
	(format t "Ээээ... что?~%"))))

(defun short-match-p (full-word short-word)
  (let ((mismatch (mismatch short-word full-word)))
    (or (not mismatch) (= mismatch (length short-word)))))

(defmacro word-dispatch (word &body clauses)
  `(when (plusp (length ,word))
     (cond
       ,@(iter (for (case-word . forms) in clauses)
	       (collecting
		 (if (not (eql case-word t))
                     `((short-match-p ,case-word ,word)
                       ,@forms)
		     `(t ,@forms)))))))
