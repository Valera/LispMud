;;; userdb.lisp

;FIXME: написать тесты!

(in-package :cl-user)
(defpackage :lispmud/userdb
  (:use :cl)
  (:import-from :alexandria #:hash-table-keys #:hash-table-values)
  (:import-from :sb-ext #:with-locked-hash-table)
  (:import-from :lispmud/core-utils #:name))
(in-package :lispmud/userdb)

;;; === online-users ===

(defvar *online-players* (make-hash-table :test 'equal :synchronized t)
  "Maps user names of online users to player objects")

(defun reset-online-users ()
  "Mark all users offile."
  (clrhash *online-players*))

(defun online-user-names ()
  "List of names of all online players."
  (sb-ext:with-locked-hash-table (*online-players*)
    (hash-table-keys *online-players*)))

(defun online-players ()
  "List of online player objects."
  (sb-ext:with-locked-hash-table (*online-players*)
    (hash-table-values *online-players*)))

(defun try-set-user-online (player)
  "Mark user as being online."
  (sb-ext:with-locked-hash-table (*online-players*)
    (if (not (nth-value 1 (gethash (name player) *online-players*)))
        (setf (gethash (name player) *online-players*) player)
        nil)))

(defun set-user-offline (player)
  "Mark user as being offline."
  (remhash (name player) *online-players*))

;;; === userdb ===

(defun users-db ()
  "List of all users database records."
  (pomo:with-transaction ()
    (pomo:query (:select 'name 'password 'online-p :from 'players))))

(defun valid-new-player-name-p (namestring)
  "Return T if new player name is valid player name, every letter is Russian."
  (every #'(lambda (c) (char-not-greaterp
			#\CYRILLIC_CAPITAL_LETTER_A c #\CYRILLIC_CAPITAL_LETTER_YA))
	 namestring))

;; Реализовать проверку на то, не залогинен ли пользователь уже.
(defun can-login (user-name password)
  "Return true if password of the user matches the password in database."
  (pomo:query (:select 'name :from 'players
		       :where (:and (:= 'name user-name) (:= 'password password)))))

(defun user-exists-p (user-name)
  "Check whether user with user-name already exists in the database."
  (pomo:query (:select 'name :from 'players
		       :where (:= 'name user-name))))

(defun register-user (user-name password)
  "Add user with given user-name and password to list of registered users.
If user with such name already exists, then do nothing and return nil."
  (pomo:with-transaction ()
    (if (not (user-exists-p user-name))
	(pomo:execute
	 (:insert-into 'players
		       :set 'name user-name 'password password 'online-p nil)))))
