(in-package :lispmud)

(defvar *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================
"
)

(defvar *user-db* (make-hash-table :test 'equal))

(defun read-value-in-range (stream prompt lower upper)
  "Reads integer value from stream from upper to lower inclusive"
  (format stream "~a: " prompt)
  (let ((val (read stream)))
    (if (and (integerp val) (<= lower val upper))
	val
	(read-value-in-range stream prompt lower upper))))

(defun prompt-read (prompt stream &key satisfy-p)
  (format stream "~a: " prompt)
  (loop
       for value = (read-line stream) then (read-line stream)
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