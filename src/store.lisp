;;; store.lisp

(in-package :lispmud)

(defvar *store* (make-hash-table :test 'equal))
(defvar *store-lock* (bt:make-lock "Store"))

(defun put-to-store (person thing)
  (bt:with-recursive-lock-held (*store-lock*)
    (push thing (gethash person *store*))))

(defun take-from-store (person thing)
  (bt:with-recursive-lock-held (*store-lock*)
    (remove thing (gethash person *store*))))

(defun items-in-store (person)
  (bt:with-recursive-lock-held (*store-lock*)
    (gethash person *store*)))

(defun load-store (file-name)
  (bt:with-recursive-lock-held (*store-lock*)
    (setf *store* (load-hash-table file-name))))

(defun dump-store (file-name)
  (bt:with-recursive-lock-held (*store-lock*)
    (dump-hash-table *store* file-name)))
