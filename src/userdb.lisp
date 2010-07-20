;;; userdb.lisp

;FIXME: Не позволять одновременных логинов!

(in-package :lispmud)

(defvar *user-db* (make-hash-table :test 'equal :synchronized t))

(defun can-login (user-name password)
  (string= (gethash user-name *user-db*) password))

(defun login (user-name password)
  (assert (can-login user-name password))
  (setf (user-name *thread-vars*) user-name))

(defun user-exists-p (user-name)
  (if (gethash user-name *user-db*)
      t
      nil))

(defun register-user (user-name password)
  (if  (gethash user-name *user-db*)
       (error "User is already registered")
       (setf (gethash user-name *user-db*) password)))

(defun dump-user-db (file-name)
  (dump-hash-table *user-db* file-name))

(defun load-user-db (file-name)
  (setf *user-db* (load-hash-table file-name)))
