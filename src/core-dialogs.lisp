;;; core-dialogs.lisp

(in-package :lispmud)

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

(defun not-empty-string-p (string)
  (string/= "" string))

;;; FIXME - end - leak
(defmacro deffsm (name &body body)
  (let ((vars (mapcar #'second body)))
    `(defun ,name ()
       (let ,vars
	 (tagbody
	    ,@(loop for state-descr in body
		   nconc (destructuring-bind (go-state var read-fun dispatch-fun) state-descr
			   (list go-state
				 `(setf ,var ,read-fun)
				 dispatch-fun))))))))
