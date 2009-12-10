;;; utils.lisp

;; Contains handy utilites, which have no direct relation
;; to LispMud project.

(in-package :lispmud)

(defun dump-hash-table (hash file-name)
  "Dump hash-table HASH to file with name FILE-NAME."
  (with-open-file (stream file-name :direction :output)
    (write (append (list :test (hash-table-test hash))
		   (hash-table-alist hash))
	   :stream stream)))

(defun load-hash-table (file-name)
  "Load hash table from file with name FILE-NAME and returns it."
  (with-open-file (stream file-name)
    (let* ((sexp (read stream))
	  (test (when (eql (first sexp) :test) (second sexp)))
	  (alist (if test (third sexp) sexp)))
     (alist-hash-table alist :test (or test 'eql)))))

(defun print-hash-table (hash)
  "Prints hash table."
  (maphash #'(lambda (key val) (format t "~a => ~a~%" key val)) hash))

(defmacro pvalue (&rest values)
  "Debug print: print each value unevaluated, then evalueted."
  `(progn
     ,@(loop for val in values collecting
	    `(format t "=debug= ~a => ~s~%" ',val ,val))))
