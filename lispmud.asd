;;; lispmud.asd

(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)

(defsystem :lispmud
  :serial t
  :depends-on (#:split-sequence #:alexandria #:iterate #:sb-queue #:postmodern
                                #:bordeaux-threads #:usocket #:cl-store
                                #:metabang-bind #:fiveam)
  :components ((:module "src"
			:serial t
			:components ((:file "core-globalvars")
				     (:file "core-utils")
                                     (:file "core-db")
				     (:file "core-mail")
				     (:file "rucase")
				     (:file "color-codes")
				     (:file "event-timer")
				     (:file "core-threadvars")
				     (:file "core-command")
				     (:file "core-FSM")
				     (:file "core-streams")
                                     (:file "tables")
				     (:file "player")
				     (:file "trigger")
				     (:file "store")
				     (:file "userdb")
				     (:file "bank")
				     (:file "exchange")
				     (:file "roll")
				     (:file "registration")
				     (:file "core-room")
				     (:file "shop")
				     (:file "core-items")
				     (:file "mob")
				     (:file "core-zone")
				     (:file "world")
;				     (:file "core-telnet")
				     (:file "core-server")
				     (:file "input-handlers")
				     (:file "command")
				     (:file "main")
                                     (:file "package")))))
