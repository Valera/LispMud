;;; lispmud.asd
 
(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)
 
(defsystem :lispmud
  :depends-on (:split-sequence :alexandria)
  :components ((:module "src" :components
		       ((:file "packages" )
			(:file "main" :depends-on ("packages"))
			(:file "utils" :depends-on ("packages"))
			(:file "bank" :depends-on ("packages"))
			(:file "sock" :depends-on ("packages"))
			(:file "command" :depends-on ("packages"))
			(:file "roll" :depends-on ("packages"))
			(:file "room" :depends-on ("packages"))
			(:file "zone" :depends-on ("packages"))
			(:file "store" :depends-on ("packages"))
			(:file "userdb" :depends-on ("packages"))
			(:file "registration" :depends-on ("packages"))))))
