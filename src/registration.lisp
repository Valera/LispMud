;;; registration.lisp

(in-package :lispmud)

(defparameter *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================
"
)

(defparameter *server-rules*
"========================================================
  Здесь должна быть портянка с правилами сервера,
   но сейчас её нет.
========================================================
"
)

(generate-fsm register-and-login-fsm start finish
    ((name :accessor name)
     (passwd :accessor passwd))
  (start
   :on-enter (format t "Введите 1, чтобы зарегистрироваться, или 2, чтобы войти в игру: ")
   :on-input (let ((n (handler-case (parse-integer input)
			(parse-error () -1))))
	       (cond
		 ((= n 2) (next-state login-name))
		 ((= n 1) (next-state register-1))
		 (t (format t "введите 1 или 2:")))))
  (login-name
   :on-enter (format t "Введите имя вашего персонажа: ")
   :on-input (progn
	       (setf (name fsm) input)
	       (next-state login-passwd)))
  (login-passwd
   :on-enter (format t "Введите пароль: ")
   :on-input
   (progn
     (setf (passwd fsm) input)
     (if (can-login (name fsm) (passwd fsm))
	 (progn
	   (format t "Приветствую тебя, ~a! Добро пожаловать!~%" (name fsm))
	   (next-state finish-login))
	 (progn
	   (format t "Игрока с таким именем и паролем не существует. Введите правильные имя и пароль или зарегистрируйтесь, если у вас их нет.~%")
	   (next-state start)))))
  (register-1
   :on-enter (progn
	       (write-string *server-rules*)
	       (format t "Принимаете ли вы эти правила? Введите ДА, если принимаете, и НЕТ, если не принимаете: "))
   :on-input
   (cond 
     ((equalp input "да")
      (format t "Мы рады привествовать вас на нашем серевере!~%")
      (next-state register-2))
     ((equalp input "нет")
      (format t "К сожалению, если вы не принимаете правила, то должны покинуть сервер. Мы надеемся, что вы передумаетее и вернётесь. :)~%")
      (next-state finish-leave))
     (t
      (format t " вы ввели ~S~%" input)
      (format t "Принимаете ли вы эти правила? Введите ДА, если принимаете и НЕТ, если не принимаете: "))))
  (register-2
   :on-enter (format t "Введите имя вашего персонажа: ")
   :on-input (if (valid-new-player-name-p input)
		 (progn
		   (setf (name fsm) input)
		   (next-state register-3))
		 (format t "Введите имя вашего персонажа: ")))
  (register-3
   :on-enter (format t "Введите пароль: ")
   :on-input (progn (if (<= 6 (length input) 20)
			(if (register-user (name fsm) input)
			    (progn
			      (format t "Отлично! Вы зарегистрировались, теперь можете начинать игру!~%")
			      (next-state finish-login))
			    (progn
			      (format t "Извините, но пользователь с таким именем уже существует. Попробуйте ещё раз~%~%")
			      (next-state register-2)))
			(format t "Пароль должен быть от 6 до 20 символов длинной. Попробуйте ещё раз: "))))
  (finish-leave)
  (finish-login
   :on-input (when (string= "облом" input)
	       (format t "~%~%К сожалению, пользователь с таким именем уже играет. Если это ты, то выйди из другого сеанса. Если это не ты, и ты думаешь, что твой пароль кем-то украден, сообщи администрации.~%~%~%")
	       (next-state start))))
