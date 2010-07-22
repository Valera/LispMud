;;; userdb.lisp

;FIXME: Не позволять одновременных логинов!

(in-package :lispmud)

(defvar *user-db* (make-hash-table :test 'equal :synchronized t))

(defun valid-new-player-name-p (namestring)
  "Return T if new player name is valid player name, every letter is Russian."
  (every #'(lambda (c) (char-not-greaterp #\А c #\Я)) namestring))

;; Реализовать проверку на то, не залогинен ли пользователь уже.
(defun can-login (user-name password)
  "Return true if user with given user-name and password exists"
  (string= (gethash user-name *user-db*) password))

;; FIXME
(defun login (user-name password)
  "Enter in game"
  (assert (can-login user-name password))
  (setf (user-name *thread-vars*) user-name))

(defun user-exists-p (user-name)
  "Check whether user with user-name already exists in database."
  (if (gethash user-name *user-db*)
      t
      nil))

(defun register-user (user-name password)
  "Add user with given user-name and password to list of registered users."
  (if  (gethash user-name *user-db*)
       (error "User is already registered")
       (setf (gethash user-name *user-db*) password)))

(defun dump-user-db (file-name)
  "Save user database to file-name."
  (dump-hash-table *user-db* file-name))

(defun load-user-db (file-name)
  "Load user database from file-name."
  (setf *user-db* (load-hash-table file-name)))
