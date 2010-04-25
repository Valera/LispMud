(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)
(defvar *world-filename* "world.lmud")
(defvar *zone-list*)
(defvar *player-output*)

(defun initialize-game ()
  (init-commands)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun main ()
  (initialize-game)
;  (temp-start-work *player-zone*)
  (run-lispmud 3004)
  
#+nil  (pvalue 12345678)
#+nil  (start-telnet-server
   +localhost+ +port+
   #'(lambda (stream)
       (let* ((*standard-input* stream)
	     (*standard-output* stream)
	     (*player-output* nil)
	 ;; FIXME: delete next line.
	     (*player-zone* (first *zone-list*))
	     (*player-room* (first (entry-rooms *player-zone*))))
	 (temp-start-work *player-zone*)
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
  (let (*player-exit-flag*)
    (loop named player-repl
       with line
       until *player-exit-flag*
       do (progn
	    (room-about *player-room*)
	    (prompt)
;;	    (pvalue (read-char-no-hang *standard-input* nil :eof))
	    (setf line (read-line *standard-input* nil :eof))
	    (if (eql line :eof)
		(return-from player-repl))
	    (exec-command line)))))
