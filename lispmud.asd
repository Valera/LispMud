;;; lispmud.asd
 
(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)
 
(defsystem :lispmud
  :depends-on (:split-sequence :alexandria :iterate)
  :components ((:module "src" :components
		       ((:file "packages" )
			(:file "utils" :depends-on ("packages"))
			(:file "main" :depends-on ("packages" "utils" "command"))
			(:file "threadvars" :depends-on ("packages" "utils"))
			(:file "bank" :depends-on ("packages" "utils"))
			(:file "sock" :depends-on ("packages" "utils"))
			(:file "command" :depends-on ("packages" "utils"))
			(:file "roll" :depends-on ("packages" "utils"))
			(:file "room" :depends-on ("packages" "utils"))
			(:file "zone" :depends-on ("packages" "utils"))
			(:file "store" :depends-on ("packages" "utils"))
			(:file "userdb" :depends-on ("packages" "utils"))
			(:file "registration" :depends-on ("packages" "utils"))
			(:file "world" :depends-on ("packages" "utils"))))))
