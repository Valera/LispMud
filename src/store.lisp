;;; store.lisp

(in-package :lispmud)

(defvar *store* (make-hash-table :test 'equal))
(defvar *store-mutex* (make-mutex :name "Store mutex"))

(defun put-to-store (person thing)
  (with-recursive-lock (*store-mutex*)
    (push thing (gethash person *store*))))

(defun take-from-store (person thing)
  (with-recursive-lock (*store-mutex*)
    (remove thing (gethash person *store*))))

(defun items-in-store (person)
  (with-recursive-lock (*store-mutex*)
    (gethash person *store*)))

(defun load-store (file-name)
  (with-recursive-lock (*store-mutex*)
    (setf *store* (load-hash-table file-name))))

(defun dump-store (file-name)
  (with-recursive-lock (*store-mutex*)
    (dump-hash-table *store* file-name)))
