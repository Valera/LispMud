;;; roll.lisp

(in-package :cl-user)
(defpackage :lispmud/roll
  (:use :cl))
(in-package :lispmud/roll)

(defun roll-once (maxval)
  "Return ranom value from 1 to maxval inclusively"
  (1+ (random maxval)))

(defun roll (maxval times)
  "Rolls dice with range from 1 to maxval times times"
  (loop
     for i from 1 to times
     summing (roll-once maxval)))
