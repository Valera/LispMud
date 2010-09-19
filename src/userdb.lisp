;;; userdb.lisp

;FIXME: Не позволять одновременных логинов!

(in-package :lispmud)

(defvar *user-db* (make-hash-table :test 'equal :synchronized t))
(defvar *user-db-lock* (make-lock "User database lock"))
(defvar *online-users*)
(defvar *online-users-lock* (make-lock "Lock for the list of online users"))

(defun reset-online-users ()
  (with-lock-held (*online-users-lock*)
    (setf *online-users* nil)))

(defun set-user-online (user-name)
  (with-lock-held (*online-users-lock*)
    (if (find (name user-name) *online-users* :key #'name :test #'string=)
	nil
	(push user-name *online-users*))))

(defun set-user-offline (user-name)
  (with-lock-held (*online-users-lock*)
    (deletef *online-users* (name user-name) :key #'name :test #'string=)))

(defun valid-new-player-name-p (namestring)
  "Return T if new player name is valid player name, every letter is Russian."
  (every #'(lambda (c) (char-not-greaterp #\А c #\Я)) namestring))

;; Реализовать проверку на то, не залогинен ли пользователь уже.
(defun can-login (user-name password)
  "Return true if password of the user matches the password in database."
  (string= (gethash user-name *user-db*) password))

(defun user-exists-p (user-name)
  "Check whether user with user-name already exists in the database."
  (if (gethash user-name *user-db*)
      t
      nil))

(defun register-user (user-name password)
  "Add user with given user-name and password to list of registered users.
If user with such name already exists, then do nothing and return nil."
  (if  (gethash user-name *user-db*)
       nil ; Caller MUST handle this case.
       (setf (gethash user-name *user-db*) password)))

(defun dump-user-db (file-name)
  "Save user database to file-name."
  (dump-hash-table *user-db* file-name))

(defun load-user-db (file-name)
  "Load user database from file-name."
  (setf *user-db* (load-hash-table file-name :test 'equal :synchronized t)))
