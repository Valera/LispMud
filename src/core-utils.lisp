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
  "Loads hash table from file with name FILE-NAME and returns it."
  (with-open-file (stream file-name)
    (let* ((sexp (read stream))
	  (test (when (eql (first sexp) :test) (second sexp)))
	  (alist (if test (third sexp) sexp)))
     (alist-hash-table alist :test (or test 'eql)))))

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

(defstruct wait-queue
  (sb-queue (sb-queue:make-queue) :type sb-queue:queue)
  (semaphore (sb-thread:make-semaphore) :type sb-thread:semaphore))

(defun enqueue (value queue)
  (sb-queue:enqueue value (wait-queue-sb-queue queue))
  (sb-thread:signal-semaphore (wait-queue-semaphore queue)))

(defun dequeue (queue)
  (sb-thread:wait-on-semaphore (wait-queue-semaphore queue))
  (sb-queue:dequeue (wait-queue-sb-queue queue)))
