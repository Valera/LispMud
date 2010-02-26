;;; registration.lisp

(in-package :lispmud)

(defparameter *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================

Введите 1 для входа в игру, или 2 для регистрации в игру"
)

(deffsm enter-game
  (go-enter enter (read-value-in-range *first-prompt* 1 2)
	    (ecase enter
	      (1 (go go-login-name))
	      (2 (go go-register-name))))
  (go-register-name rname (prompt-read "Введите имя: ")
		    (go go-register-pass1))
  (go-register-pass1 rpass1 (prompt-read "Введите пароль: ")
		     (go go-register-pass2))
  (go-register-pass2 rpass2 (prompt-read "Введите пароль ещё раз: ")
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
  (go-login-name lname (prompt-read "Имя: ")
		 (go go-login-pass))
  (go-login-pass lpass (prompt-read "Пароль: ")
		 (if (can-login lname lpass)
		     (login lname lpass)
		     (progn
		       (format t "Имя и пароль не соответствуют друг другу. Повторите ввод.~%")
		       (go go-login-name)))))
