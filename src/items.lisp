;;; items.lisp

(in-package :lispmud)

(defclass item ()
  ((name :accessor name)
   (description :accessor description)
   (price :accessor price)))
