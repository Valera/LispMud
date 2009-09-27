;;; lispmud.asd
 
(defpackage :lispmud-system
  (:use :cl :asdf))

(in-package :lispmud-system)
 
(defsystem :lispmud
  :depends-on (:split-sequence)
  :components ((:file "src/packages" )
	       (:file "src/bank" :depends-on ("src/packages"))
	       (:file "src/command" :depends-on ("src/packages"))
	       (:file "src/roll" :depends-on ("src/packages"))
	       (:file "src/room" :depends-on ("src/packages"))
	       (:file "src/zone" :depends-on ("src/packages"))
	       (:file "src/registration" :depends-on ("src/packages"))))
