;;; mudsketcher.asd
 
(defpackage :mudsketcher-system
  (:use :cl :asdf))

(in-package :mudsketcher-system)
 
(defsystem :mudsketcher
  :serial t
  :depends-on (:lispmud :split-sequence :alexandria :iterate :cl-gtk2-gtk :cl-cairo2 :cl-gtk2-cairo)
  :components ((:module "mudsketcher"
			:serial t
			:components ((:file "packages" )
				     (:file "mudsketcher")))))

