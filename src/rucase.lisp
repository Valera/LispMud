;;; rucase.lisp
;;; Printing of Russian cases, sexes, times etc.

(in-package :lispmud)

(defvar *case-hash* (make-hash-table :test 'equal))

(defun rucase (word case sex)
  (gethash (list word case sex) *case-hash*))