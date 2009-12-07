;;; definitions.lisp

(in-package :lispmud)

(defclass database-connection
    (cl-perec:database-mixin
     cl-rdbms:postgresql-postmodern)
  ())

(defparameter
    cl-perec:*database*
  (make-instance 'database-connection
		 :connection-specification
		 '(:database  "repl"
		   :user-name "testuser"
		   :password  "testpass")))

(defpclass* mud-user ()
  ((name :type (text 16) :unique t)
   (password :type (text 16))
   (money 0 :type integer-64-type)
   (banned-p :type (text 1))
   (logined-p :type (text 1))))

(defpclass* mud-char ()
  ((name :type (text 16) :unique t)
   (money-in-bank :type integer-64-type)
   (class :type (text 16))
   (race :type (text 16))
   (level :type (intege))
   (experience (integer))))

(defpclass* item-type ()
  ((cost :type integer-32-type)
   (name :type (text 30))
   (description (text 255))))

(defpclass* item ()
  ((timer :type integer)
   (state :undefined :type (member :in-store :in-exchange :in-inventory :undefined))))

(defpclass* stock ()
  ((price :type integer)))

(defpclass* skill ()
  ((level :type integer)
   (name :type (text 40))
   (description :type (text 400))))
