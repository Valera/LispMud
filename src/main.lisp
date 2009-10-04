(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)

(defun main ()
  (init-command-table)
  (start-telnet-server
   +localhost+ +port+
   #'(lambda (stream)
       (let ((*standard-input* stream)
	     (*standard-output* stream)
	     (*thread-vars* (make-instance 'thread-vars)))
	 ;; FIXME: delete next line.
	 (setf (cur-zone *thread-vars*) (load-zone "zone.test"))
	 (format t "~a~%"  (room-list (cur-zone *thread-vars*)))
	 (setf (cur-room *thread-vars*) (first (room-list (cur-zone *thread-vars*))))
	 (player-loop)))))

(defun player-loop ()
  (loop with line
     until (equal :eof (setf line (read-line)))
     do (progn
	  (format t "Line read~%")
	  (format t "В комнате ~a~%" (description (cur-room *thread-vars*)))
	  (exec-command line))))