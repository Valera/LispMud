(in-package :lispmud)

(defun enter-alpha-password (client input)
  (if (string= *alpha-version-password* (string-trim '(#\Space #\Newline #\Return) input))
      (progn
	(setf (player-state client) 'login)
	(format t "Пароль принят.~%")
	(setf  (register-and-login-fsm client) (make-instance 'register-and-login-fsm))
	(push-input-handler 'registration-and-login-handler))
      (format t "Неверный пароль. Повторите ввод: ")))

(defun registration-and-login-handler (client input)
  (with-slots ((fsm register-and-login-fsm)) client
    (process-input1 fsm (string-trim '(#\Space #\Newline #\Return) input))
    (when (eql (current-state fsm) 'finish-login)
      (setf *player* (make-instance 'player :name (name fsm) :output   *standard-output*))
      (if (set-user-online *player*)
	  (progn
	    (push *player* (players *player-room*))
	    (if (have-mail-for *player*)
		(format t "У вас есть ~a непрочитанных письма." (length (have-mail-for *player*))))
	    (room-about *player-room*)
	    (pop-input-handler)
	    (push-input-handler 'terminal-input-handler))
	  (progn
	    (process-input1 fsm "облом")
	    (setf *player* nil)))))
       ;; FIXME: Выход без регистрации.
#+nil	 (error (condition) (format t "Command erred with condition ~a~%" condition)))

(defvar *room-changed*)
(defun terminal-input-handler (client input)
  (declare (ignore client))
  (let ((*room-changed* nil))
    (exec-command (string-trim '(#\Space #\Newline #\Return) input))
    (if *room-changed*
	(room-about *player-room*))))

