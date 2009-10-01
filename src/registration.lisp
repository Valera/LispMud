(in-package :lispmud)

(defvar *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================
"
)

(defvar *user-db* (make-hash-table :test 'equal))

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

(defun enter-to-game (stream)
  (format stream "~a~%" *first-prompt*)
  (ecase (read-value-in-range stream "Введите 1 для входа, 2 для регистрации" 1 2)
    (1 (let ((user (prompt-read "Введите логин" stream :satisfy-p #'not-empty-string-p))
	     (password  (prompt-read "Введите пароль" stream :satisfy-p #'not-empty-string-p)))
	 (if (string= (gethash user *user-db*) password)
	     (format stream "Ура!~%")
	     (format stream "Не ура. :(~%"))))
    (2 (let ((user (prompt-read "Введите имя пользователя" stream)))
	 (+ 1 1)))))

(defun register (stream)
  (let ((user (prompt-read "Введите логин" stream :satisfy-p #'not-empty-string-p))
	(password  (prompt-read "Введите пароль" stream :satisfy-p #'not-empty-string-p)))
    (if (gethash user *user-db*)
	(error "User already registered"))
    (setf (gethash user *user-db*) password)))
    ;(login user password)))

(defun login (stream)
  (let ((user (prompt-read "Введите логин" stream :satisfy-p #'not-empty-string-p))
	(password  (prompt-read "Введите пароль" stream :satisfy-p #'not-empty-string-p)))
    (unless (gethash user *user-db*)
      (error "User is not registered"))
    (unless (string= (gethash user *user-db*) password)
      (error "Wrong password"))))

(defun print-hash (hash)
  (maphash #'(lambda (key val) (format t "~a => ~a~%" key val)) hash))

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

(defun can-login (a b) nil)

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
			 (progn (format t "Регистрация завершена.~%")
				(return-from enter-game))
			 (progn (format t "Пароли не совпадают. Пожалуйста, пройдите регистрацию заново.~%")
				(go go-register-name))))
  (go-login-name lname (prompt-read "Имя")
		 (go go-login-pass))
  (go-login-pass lpass (prompt-read "Пароль")
		 (if (can-login lname lpass)
		     (login lname lpass)
		     (go go-login-name))))
