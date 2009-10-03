;;; userdb.lisp

(in-package :lispmud)

(defvar *user-db* (make-hash-table :test 'equal))
(defvar *user-db-mutex* (make-mutex :name "User DB mutex"))

(defun can-login (user-name password)
  (with-recursive-lock (*user-db-mutex*)
    (if (string= (gethash user-name *user-db*) password)
	t
	nil)))

(defun login (user-name password)
  (assert (can-login user-name password))
  (setf (user-name *player-info*) user-name))

(defun register-user (user-name password)
  (with-recursive-lock (*user-db-mutex*)
    (setf (gethash user-name *user-db*) password)))

(defun dump-user-db ()
  ())

(defun load-user-db ()
  ())
