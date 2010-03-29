;;; lispmud.asd
 
(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)
 
(defsystem :lispmud
  :serial t
  :depends-on (:split-sequence :alexandria :iterate :sb-queue 
			       :bordeaux-threads :usocket)
  :components ((:module "src"
			:serial t
			:components ((:file "packages" )
				     ;; core
				     (:file "core-utils")
				     (:file "core-threadvars")
				     (:file "core-command")
				     (:file "core-dialogs")
				     (:file "core-room")
				     (:file "core-zone")
				     (:file "core-telnet")
				     ;; content
				     (:file "mob")
				     (:file "main")
				     (:file "exchange")
				     (:file "bank")
				     (:file "roll")
				     (:file "store")
				     (:file "userdb")
				     (:file "registration")
				     (:file "command")
				     (:file "world")))))
