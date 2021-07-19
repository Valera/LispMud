;;; utils.lisp

;; Contains handy utilites, which have no direct relation
;; to LispMud project.

(in-package :cl-user)
(defpackage :lispmud/core-utils
  (:use :cl)
  (:import-from :alexandria
                #:hash-table-alist #:alist-hash-table #:with-unique-names)
  (:import-from :iter
                #:for #:in #:with
                #:iter #:defmacro-clause #:collect))
(in-package :lispmud/core-utils)

(defun dump-hash-table (hash file-name)
  "Dump hash-table HASH to file with name FILE-NAME."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
    (write (append (list :test (hash-table-test hash))
		   (hash-table-alist hash))
	   :stream stream)
    ;; Add newline at end of file.
    (terpri stream)))

(defun load-hash-table (file-name &key (test 'eql) synchronized)
  "Loads hash table from file with name FILE-NAME and returns it."
  (handler-case
      (with-open-file (stream file-name)
	(let* ((sexp (read stream))
	       (local-test (when (eql (first sexp) :test) (second sexp)))
	       (alist (if test (nthcdr 2 sexp) sexp)))
	  (alist-hash-table alist :test (or local-test test))))
    (file-error () (make-hash-table :test test :synchronized synchronized))))

(defun print-hash-table (hash)
  "Prints hash table to *standard-output*"
  (maphash #'(lambda (key val) (format t "~a => ~a~%" key val)) hash))

(defmacro pvalue (&rest values)
  "Debug print: print each value unevaluated, then evaluated."
  `(progn
     ,@(loop for val in values collecting
	    `(format t "=debug= ~a => ~s~%" ',val ,val))))

(defmacro with-hash-table-value ((variable key hash-table) &body forms)
  "Binds variable to (gethash key hash-table) and saves it after evaluating
forms to the hash-table."
  `(let ((,variable (gethash ,key ,hash-table)))
     ,@forms
     (setf (gethash ,key ,hash-table) ,variable)))

(defmacro string-join (&rest strings)
  "Joins string and characters at compile time"
  (apply #'concatenate 'string (mapcar #'string strings)))

(defmacro-clause (FOR var IN-MATRIX m)
  "Iterate macro: iteration over elements of matrix m."
  (with-unique-names (matr index lX lY x y)
    `(progn
       (with ,matr = ,m)
       (with ,lY = (array-dimension ,matr 1))
       (with ,lX = (array-dimension ,matr 0))
       (for ,index from 0 below (* ,LY ,LX))
       (for ,var = (multiple-value-bind (,y ,x) (floor ,index ,lX)
		     (list ,y ,x (aref ,matr ,y ,x)))))))

(defmacro with-variables-from (place vars-list &body forms)
  `(let
       ,(iter (for var in vars-list)
	      (collect `(,var (getf ,place ',var))))
     (unwind-protect
	  (progn ,@forms)
       ,@(iter (for var in vars-list)
	       (collect `(setf (getf ,place ',var) ,var))))))

(defun not-empty-string-p (string)
  (string/= "" string))

(defun read-value-in-range (prompt lower upper)
  "Reads integer value from stream from upper to lower inclusive"
  (format t "~a: " prompt)
  (let ((val (read)))
    (if (and (integerp val) (<= lower val upper))
	val
	(read-value-in-range prompt lower upper))))

(defun prompt-read (prompt &key satisfy-p)
  (format t "~a" prompt)
  (loop
       for value = (read-line) then (read-line)
       when (or (not satisfy-p) (funcall satisfy-p value))
       do (return value)))

(defgeneric name (object)) ;; accessor for 'name slot.
(defmethod name ((name string))
  name)
