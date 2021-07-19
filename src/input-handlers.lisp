(in-package :cl-user)
(defpackage :lispmud/input-handlers
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:use :lispmud/core-globalvars)
  (:import-from :lispmud/core-utils #:name #:pvalue #:with-variables-from)
  (:import-from :lispmud/core-command #:exec-command)
  (:import-from :lispmud/core-fsm #:process-input1 #:current-state)
  (:import-from :lispmud/userdb #:try-set-user-online)
  ; (:import-from :lispmud/player #:out-stream)
  (:import-from :lispmud/mob #:have-mail-for)
  (:import-from :lispmud/player #:player)
  (:import-from :lispmud/core-room #:room-about #:players)
  (:import-from :lispmud/registration #:finish-login)
  (:import-from :lispmud/core-server #:push-input-handler #:pop-input-handler
                #:player-state #:out-stream #:first-client-interaction
                #:register-and-login-fsm #:client #:globvars))
(in-package :lispmud/input-handlers)

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
;   (format t "#1~%")
    (process-input1 fsm (string-trim '(#\Space #\Newline #\Return) input))
;   (format t "#2 ~a ~a ~%" (current-state fsm) 'finish-login)
    (when (eql (current-state fsm) 'finish-login)
      (setf *player* (make-instance 'player :name (name fsm) :output   *standard-output*))
      (if (try-set-user-online *player*)
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

(defmethod first-client-interaction ((client client))
  ;; TODO: add choosing encoding
  (write-line "Добро пожаловать." (out-stream client))
  (with-variables-from (globvars client)
      (*standard-output* *client*)
;   (pvalue (globvars client))
;   (pvalue *standard-output* *client*)
    (if *alpha-version-password*
	(progn
	  (format t "Введите пароль альфа-версии: ")
	  (push-input-handler 'enter-alpha-password))
	(progn
	  (setf (register-and-login-fsm client)
		(make-instance 'register-and-login-fsm))
	  (push-input-handler 'registration-and-login-handler)))))
