;;; userdb.lisp

;FIXME: написать тесты!

(in-package :lispmud)

(defun reset-online-users ()
  "Mark all users offile."
  (pomo:with-transaction ()
    (pomo:execute (:update 'players :set 'online-p nil))))

(defun online-user-names ()
  (pomo:with-transaction ()
    (pomo:execute (:select 'name :from 'players :where (:= 'online-p t)))))

(defun users-db ()
  "List of all users database records."
  (pomo:with-transaction ()
    (pomo:query (:select 'name 'password 'online-p :from 'players))))

(defun try-set-user-online (player)
  "Mark user as being online."
  (pomo:with-transaction ()
    (if (pomo:query (:select 'online-p :from 'players
                             :where (:and (:= 'name (name player)) (:= 'online-p nil))))
	;; Then: register user as beeing online, set online-p to nil.
        (pomo:execute (:update 'players :set 'online-p t :where (:= 'name (name player))))
	;; Else: user already online, explicitly return nil.
	nil)))

(defun set-user-offline (player)
  "Mark user as being offline."
  (pomo:with-transaction ()
    (pomo:execute (:update 'players :set 'online-p nil
			   :where (:= 'name (name player))))))

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
