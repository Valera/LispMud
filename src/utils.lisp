;;; utils.lisp

;; Contains handy utilites, which have no direct relation
;; to LispMud project.

(in-package :lispmud)

(defun dump-hash-table (hash file-name)
  (with-open-file (stream file-name :direction :output)
    (write (append (list :test (hash-table-test hash))
		   (hash-table-alist hash))
	   :stream stream)))

(defun load-hash-table (file-name)
  (with-open-file (stream file-name)
    (let* ((sexp (read stream))
	  (test (when (eql (first sexp) :test) (second sexp)))
	  (alist (if test (third sexp) sexp)))
     (alist-hash-table alist :test (or test 'eql)))))
