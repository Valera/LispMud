(in-package :lispmud)

(defparameter *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================

Введите 1 для входа в игру, или 2 для регистрации в игру"
)

(defun read-value-in-range (prompt lower upper)
  "Reads integer value from stream from upper to lower inclusive"
  (format t "~a: " prompt)
  (let ((val (read)))
    (if (and (integerp val) (<= lower val upper))
	val
	(read-value-in-range prompt lower upper))))

(defun prompt-read (prompt &key satisfy-p)
  (format t "~a: " prompt)
  (loop
       for value = (read-line) then (read-line)
       when (or (not satisfy-p) (funcall satisfy-p value))
       do (return value)))

(defun not-empty-string-p (string)
  (string/= "" string))

;;; FIXME - end - leak
(defmacro deffsm (name &body body)
  (let ((vars (mapcar #'second body)))
    `(defun ,name ()
       (let ,vars
	 (tagbody
	    ,@(loop for state-descr in body
		   nconc (destructuring-bind (go-state var read-fun dispatch-fun) state-descr
			   (list go-state
				 `(setf ,var ,read-fun)
				 dispatch-fun)))
	    )))))

(deffsm enter-game
  (go-enter enter (read-value-in-range *first-prompt* 1 2)
	    (ecase enter
	      (1 (go go-login-name))
	      (2 (go go-register-name))))
  (go-register-name rname (prompt-read "Введите имя")
		    (go go-register-pass1))
  (go-register-pass1 rpass1 (prompt-read "Введите пароль")
		     (go go-register-pass2))
  (go-register-pass2 rpass2 (prompt-read "Введите пароль ещё раз")
		     (if (string= rpass1 rpass2)
			 (if (user-exists-p rname)
			     (progn
			       (format t "Извините, пользователь с таким именем уже существует.~%")
			       (go go-register-name))
			     (progn 
			       (format t "Регистрация завершена.~%")
			       (register-user rname rpass1)
			       (go  go-enter)))
			 (progn (format t "Пароли не совпадают. Пожалуйста, пройдите регистрацию заново.~%")
				(go go-register-name))))
  (go-login-name lname (prompt-read "Имя")
		 (go go-login-pass))
  (go-login-pass lpass (prompt-read "Пароль")
		 (if (can-login lname lpass)
		     (login lname lpass)
		     (progn
		       (format t "Имя и пароль не соответствуют друг другу. Повторите ввод.~%")
		       (go go-login-name)))))
