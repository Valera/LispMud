;;; lispmud.asd
 
(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)
 
(defsystem :lispmud
  :serial t
  :depends-on (:split-sequence :alexandria :iterate :sb-queue :postmodern
			       :bordeaux-threads :usocket :cl-store)
  :components ((:module "src"
			:serial t
			:components ((:file "packages" )
				     ;; core
				     (:file "core-utils")
				     (:file "core-mail")
				     (:file "rucase")
				     (:file "color-codes")
				     (:file "event-timer")
				     (:file "core-threadvars")
				     (:file "trigger")
				     (:file "core-command")
				     (:file "core-FSM")
				     (:file "core-items")
				     (:file "core-room")
				     (:file "core-zone")
;				     (:file "core-telnet")
				     (:file "core-streams")
				     (:file "core-server")
				     ;; content
				     (:file "mob")
				     (:file "main")
				     (:file "exchange")
				     (:file "bank")
				     (:file "roll")
				     (:file "store")
				     (:file "userdb")
				     (:file "registration")
				     (:file "player")
				     (:file "command")
				     (:file "world")))))
