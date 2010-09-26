(in-package :cl-user)

(defpackage :mudsketcher
  (:use :cl :alexandria :iterate :gtk :cl-cairo2 :cl-gtk2-cairo :gdk :gobject)
  (:shadowing-import-from :cl-cairo2 :scale :rotate :rectangle :pointer))
