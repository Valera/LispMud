;;; store.lisp

(in-package :lispmud)

(defvar *store* (make-hash-table :test 'equal))
(defvar *store-mutex* (make-mutex))

(defun put-to-store (person thing)
  ;(assert (gethash person *store*))
  (push thing (gethash person *store*)))

(defun take-from-store (person thing)
  ;(assert (gethash person *store*))
  (remove thing (gethash person *store*)))

(defun items-in-store (person)
  ;(assert (gethash person *store*))
  (gethash person hash-table))

(defun load-store ()
  ())

(defun save-store ()
  ())
