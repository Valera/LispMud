(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)
(defvar *world-filename* "world.lmud")
(defvar *zone-list* nil)

(defun initialize-game ()
  (init-command-table)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun main ()
  (initialize-game)
  (pvalue 12345678)
  (start-telnet-server
   +localhost+ +port+
   #'(lambda (stream)
       (let ((*standard-input* stream)
	     (*standard-output* stream)
	     #-(and) (*thread-vars* (make-instance 'thread-vars)))
	 ;; FIXME: delete next line.
	 (setf *player-zone* (first *zone-list*))
	 ;(format t "~a~%"  (room-list  *player-zone*))
	 ;(setf *player-room* (first (room-list *player-room*)))
	 (setf *player-room* (first (entry-rooms *player-zone*)))
	 (player-loop)
	 (close stream)))))

(defun prompt ()
  (format t "~:[~;с~]~:[~;ю~]~:[~;з~]~:[~;в~]>"
	  (north-exit *player-room*)
	  (south-exit *player-room*)
	  (west-exit *player-room*)
	  (east-exit *player-room*))
  (force-output))


(defun player-loop ()
  (loop with line
     until (or (equal :eof (setf line (read-line *standard-input* nil :eof)))
	        *player-exit-flag*)
     do (progn
	  (format t "Line read~%")
	  (format t "В комнате ~a~%" (description *player-room*))
	  (prompt)
	  (exec-command line))))
