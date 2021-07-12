;;; core-db.lisp

(in-package :lispmud)

(defparameter *dbspec* '(:name "lispmud"))

#+nil
(defmacro with-items-collection (name &body body)
  (let ((blog-symbol (gensym)))
    `(let* ((,blog-symbol (apply 'make-instance 'mongo:database *dbspec*))
            (,name (mongo:collection ,blog-symbol "items")))
       (unwind-protect
            (progn ,@body)
         (mongo:close-database ,blog-symbol)))))
