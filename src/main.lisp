(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)

(defun initialize-game ()
  (init-command-table))

(defun main ()
  (initialize-game)
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
	 (format t "*t-v*0 ~a~%" *thread-vars*)
	 (player-loop)
	 (close stream)))))

(defun player-loop ()
  (loop with line
     until (or (equal :eof (setf line (read-line *standard-input* nil :eof)))
	       (end-p *thread-vars*))
     do (progn
	  (format t "Line read~%")
	  (format t "*t-v* ~a~%" *thread-vars*)
	  (format t "В комнате ~a~%" (description (cur-room *thread-vars*)))
	  (exec-command line))))
