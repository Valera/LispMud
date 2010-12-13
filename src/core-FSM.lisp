;;; core-FSM.lisp
;;;
;;; Contains macro for convinient definition of final state machines.
;;;

(in-package :lispmud)

(defclass fsm ()
  ((state-list :accessor state-list)
   (current-state :accessor current-state)
   (variable-bindings :accessor variable-bindings)
   (enter-funs :accessor enter-funs :initform (make-hash-table))   ; Maps states to functions called on enter.
   (leave-funs :accessor leave-funs :initform (make-hash-table)))) ; Maps states to functions called when leaving states.

(defgeneric process-data (fsm input))

;; Как быть с протечками аргументов fsm и fsm, input?
;; FIXME: проверить, существуют ли initial-state-name final-state-name
(defmacro generate-fsm (name initial-state-name final-state-name slots &body state-forms)
  (declare (ignore final-state-name))
  (let ((enter-functions (make-hash-table))
	(enter-gensyms (make-hash-table))
	(leave-functions (make-hash-table))
	(leave-gensyms (make-hash-table))
	states-list
	ecase-forms)
    (iter (for state-form in state-forms)
	  (destructuring-bind (state &key on-enter on-leave on-input)
	      state-form
	    (pvalue on-enter on-leave on-input)
	    (when on-enter
	      (let ((gensym (gensym (concatenate 'string (string state) "-STATE-ENTER-"))))
		(setf (gethash state enter-gensyms) gensym)
		(setf (gethash state enter-functions)
		      (list gensym '(fsm) '(declare (ignorable fsm)) on-enter))))
	    (when on-leave
	      (let ((gensym (gensym (concatenate 'string (string state) "-STATE-LEAVE-"))))
		(setf (gethash state leave-gensyms) gensym)
		(setf (gethash state leave-functions)
		      (list gensym '(fsm) '(declare (ignorable fsm)) on-leave))))
	    (push state states-list)
					;(print-hash-table enter-functions)
	    (push (list state on-input) ecase-forms)))
    (setf states-list (nreverse states-list))
    (setf ecase-forms (nreverse ecase-forms))
    `(progn
       (defclass ,name (fsm) ,slots)
       (defmethod initialize-instance :after ((instance ,name) &rest initargs)
	 (declare (ignore initargs))
	 (with-slots (current-state state-list) instance
	   (setf current-state ',initial-state-name)
	   (setf state-list ',states-list))
	 ,(if (gethash initial-state-name enter-functions)
	      `(funcall #'(lambda ,@(rest (gethash initial-state-name enter-functions))) instance)))
       (defmethod process-input1 ((fsm ,name) input)
	 (flet ,(concatenate 'list
			     (iter (for (nil definition) in-hashtable enter-functions)
				   (collecting definition))
			     (iter (for (nil definition) in-hashtable leave-functions)
				   (collecting definition)))
					;(with-gensyms (enters-plist leaves-plist)
					;(macrolet ((next-state (x)
					;		`(progn (setf (current-state fsm) ',x)
					;			nil)))
	   (ecase (current-state fsm)
	     ,@(iter (for form in ecase-forms)
		     (for state = (first form))
		     (collecting
		       `(,state
			 (macrolet ((next-state (x)
				      (let ((enter-plist ',(hash-table-plist enter-gensyms))
					    (states ',states-list))
					;,(codegen-next-state 'x state leave-functions leave-gensyms enter-functions enter-gensyms)))
					;  ',@'(,state)
					`(progn ,@'(,(if (gethash state leave-functions) `(,(gethash state leave-gensyms) fsm)))
						(setf (current-state fsm) ',x)
						,@(assert (member x states) () "~S is not a state of this FSM" x)
						,@(if (getf enter-plist x) `((,(getf enter-plist x) fsm)))
						(return-from process-input1)))))
			   ,@(rest form)))))))))))

;; Конечный автомат для отладки...
(generate-fsm simple-fsm start finish
    ((name :accessor name)
     (passwd :accessor passwd))
  (start
   :on-enter (format t "Введите 1, чтобы зарегистрироваться, или 2, чтобы войти в игру")
   :on-input (let ((n (parse-integer input)))
	       (cond
		 ((= n 1) (next-state login-name))
		 (t (format t "введите 1 или 2:")))))
  (login-name
   :on-enter (format t "Введите имя вашего персонажа: ")
   :on-input (progn
	       (setf (name fsm) input)
	       (next-state finish))
   :on-leave (format t "Good-bye!"))
  (finish :on-enter (format t "Finish!")))
