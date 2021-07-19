;;; core-db.lisp

#+(or)
(in-package :lispmud)

#+(or)
(defparameter *dbspec* '(:name "lispmud"))

#+(or)
(defmacro with-items-collection (name &body body)
  (let ((blog-symbol (gensym)))
    `(let* ((,blog-symbol (apply 'make-instance 'mongo:database *dbspec*))
            (,name (mongo:collection ,blog-symbol "items")))
       (unwind-protect
            (progn ,@body)
         (mongo:close-database ,blog-symbol)))))
