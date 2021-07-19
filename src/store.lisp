;;; store.lisp

(in-package :cl-user)
(defpackage :lispmud/store
  (:use :cl)
  (:import-from :bt #:make-lock #:with-recursive-lock-held)
  (:import-from :alexandria #:deletef)
  (:import-from :lispmud/core-utils #:name))
(in-package :lispmud/store)

(defvar *store* (make-hash-table :test 'equal))
(defvar *store-lock* (bt:make-lock "Store"))

(defun put-to-store (person thing)
  (bt:with-recursive-lock-held (*store-lock*)
    (let ((person (if (stringp person) person (name person))))
;      (with-collection
      (push thing (gethash person *store*)))))

(defun take-from-store (person thing-name)
  (bt:with-recursive-lock-held (*store-lock*)
    (let ((thing (find thing-name (gethash person *store*) :key #'name :test #'string-equal)))
      (deletef (gethash person *store*) thing)
      thing)))

(defun items-in-store (person)
  (bt:with-recursive-lock-held (*store-lock*)
    (gethash person *store*)))

(defun load-store (file-name)
  (bt:with-recursive-lock-held (*store-lock*)
    (handler-case
	(setf *store* (cl-store:restore file-name))
      (sb-int:simple-file-error ()
	(setf *store* (make-hash-table :test 'equal))))))

(defun dump-store (file-name)
  (bt:with-recursive-lock-held (*store-lock*)
    (cl-store:store *store* file-name)))
