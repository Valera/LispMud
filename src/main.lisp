(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)

(defun main ()
  (init-command-table)
  (start-telnet-server
   +localhost+ +port+
   #'(lambda (stream)
       (let ((*standard-input* stream)
	     (*standard-output* stream))
	 (loop with line
	    until (equal :eof (setf line (read-line)))
	    do (progn
		 (format t "Line read~%")
		 (exec-command line)))))))
