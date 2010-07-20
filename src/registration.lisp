;;; registration.lisp

(in-package :lispmud)

(defparameter *first-prompt*
"========================================================
      Добро пожаловать в лисповый мад!
========================================================

Введите 1 для входа в игру, или 2 для регистрации в игру"
)

(defparameter *server-rules*
"========================================================
  Здесь должна быть портянка с правилами сервера,
   но сейчас её нет.
========================================================
"
)

#+ nil (defun lispmud-process-line (input-line client)
  (ecase game-stage
    ('login ...)
    ('register ...)
    ('play ...)))
;;(defun fsm (

(defclass fsm ()
  ((state-list :accessor state-list)
   (current-state :accessor current-state)
   (variable-bindings :accessor variable-bindings)
   (enter-funs :accessor enter-funs :initform (make-hash-table))   ; Maps states to functions called on enter.
   (leave-funs :accessor leave-funs :initform (make-hash-table)))) ; Maps states to functions called when leaving states.

(defgeneric process-data (fsm input))

#+ nil (defmacro generate-fsm (name initial-state-name final-state-name slots &body state-clauses)
  `(progn
     (defclass ,name (fsm) ,slots)
     (defmethod initialize-instance :after ((instance ,name) &rest initargs)
       (setf (slot-value instance 'current-state) ,initial-state-name))
     (defmethod process-data ((fsm ,name) input)
       (macrolet ((next-state (x)
		    `(progn (print 123)
			    (setf (current-state fsm) ',x)
			    (go ,x))))
	 (tagbody
	    (ecase (current-state fsm)
	      ,@(iter (for i in state-clauses)
		    (collect `(,(first i) (go ,(first i))))))
	    ;,(gensym (concatenate 'string (string name) "-INITIAL-STATE"))
	    ;(go (current-state fsm))
	    ,@(iter (for i in state-clauses)
		    (nconcing i)))))))

#+nil ;; Otlojeno poka
(defun codegen-next-state (next-state state leave-functions leave-gensyms enter-functions enter-gensyms)
  `(progn ,@(if (gethash state leave-functions) `((,(gethash state leave-gensyms))))
	  (setf (current-state fsm) ,next-state)
	  ,@(if (gethash next-state enter-functions) `((,(gethash state enter-gensyms))))
	  (return-from process-input)))

;;#+nil ;; Otlojeno poka
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
	   (setf current-state ,initial-state-name)
	   (setf state-list ',states-list)))
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
				      (let ((enter-plist ',(hash-table-plist enter-gensyms)))
					;,(codegen-next-state 'x state leave-functions leave-gensyms enter-functions enter-gensyms)))
					;  ',@'(,state)
					`(progn ,@'(,(if (gethash state leave-functions) `(,(gethash state leave-gensyms) fsm)))
						(setf (current-state fsm) ',x)
						,@(if (getf enter-plist x) `((,(getf enter-plist x) fsm)))
						(return-from process-input1)))))
			   ,@(rest form))))))))))))

;; Добавить проверки правильности аргументов.
#+nil
(defmacro generate-dinamic-fsm (name initial-state-name final-state-name slots &body state-forms)
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
	 (with-slots (current-state state-list enter-funs leave-funs) instance
	   (setf current-state ,initial-state-name)
	   (setf state-list ',states-list)
	   ,@(iter (for (state definition) in-hashtable enter-functions)
		   (collecting `(setf (gethash ',state enter-funs) #'(lambda ,@(rest definition)))))
	   ,@(iter (for (state definition) in-hashtable leave-functions)
		   (collecting `(setf (gethash ',state leave-funs) #'(lambda ,@(rest definition)))))))
       (defmethod process-input1 ((fsm ,name) input)
	 (ecase (current-state fsm)
	   ,@(iter (for form in ecase-forms)
		   (for state = (first form))
		   (collecting
		     `(,state
		       (macrolet ((next-state (x)
					;,(codegen-next-state 'x state leave-functions leave-gensyms enter-functions enter-gensyms)))
				    `(progn
				       (with-slots (enter-funs leave-funs) fsm
					 (let ((leave-fun (gethash ',@'(,state) leave-funs)))
					   (when leave-fun
					     (funcall leave-fun fsm)))
					 (setf (current-state fsm) ',x)
					 (let ((enter-fun  (gethash ',x enter-funs)))
					   (when enter-fun
					     (funcall enter-fun fsm)))
					 (return-from process-input1)))))
			 ,@(rest form))))))))))

(defun valid-new-player-name-p (namestring)
  (every #'(lambda (c) (char-not-lessp #\а c #\я)) namestring))

(generate-fsm simple-fsm 'start 'finish
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

(generate-fsm register-and-login-fsm 'start 'finish
    ((name :accessor name)
     (passwd :accessor passwd))
  (start
   :on-enter (format t "Введите 1, чтобы зарегистрироваться, или 2, чтобы войти в игру")
   :on-input (let ((n (parse-integer input)))
	       (cond
		 ((= n 1) (next-state login-name))
		 ((= n 2) (next-state register))
		 (t (format t "введите 1 или 2:")))))
  (login-name
   :on-enter (format t "Введите имя вашего персонажа: ")
   :on-input (progn
	       (setf (name fsm) input)
	       (next-state login-passwd)))
  (login-passwd
   :on-enter (format t "Введите пароль: ")
   :on-input
   (progn
     (setf (passwd fsm) input)
     (if (can-login (name fsm) (passwd fsm))
	 (progn
	   (format t "Приветствую тебя, ~a! Добро пожаловать!" (name fsm))
	   (next-state finish-login))
	 (progn
	   (format t "Игрока с таким именем и паролем не существует. Введите правильные имя и пароль или зарегистрируйтесь, если у вас их нет.")
	   (next-state start)))))
  (register-1
   :on-enter (progn
	       (write-string *server-rules*)
	       (format t "Принимаете ли вы эти правила? Введите ДА, если принимаете и НЕТ, если не принимаете."))
   :on-input
   (cond 
     ((equalp input "да")
      (format t "Мы рады привествовать вас на нашем серевере!")
      (next-state resister-2))
     ((equalp input "нет")
      (format t "К сожалению, если вы не принимаете правила, то должны покинуть сервер. Мы надеемся, что вы передумаетее и вернётесь. :)")
      (next-state final-leave))
     (t
      (format t "Принимаете ли вы эти правила? Введите ДА, если принимаете и НЕТ, если не принимаете."))))
  (register-2
   :on-enter (format t "Введите имя вашего персонажа.")
   :on-input (if (valid-new-player-name-p input)
		 (progn
		   (setf (name fsm) input)
		   (next-state register-3))
		 (format t "Введите имя вашего персонажа.")))
  (register-3
   :on-enter (format t "Введите пароль:")
   :on-input (progn (if (<= 6 (length input) 20)
			(progn
			  (format t "Отлично! Вы зарегистрировались, теперь можете начинать игру!")
			  (next-state finish-login)))
		    (format t "Пароль должен быть от 6 до 20 символов длинной. Попробуйте ещё раз: ")))
  (finish-leave)
  (finish-login))


#+ nil (deffsm enter-game
  (go-enter enter (read-value-in-range *first-prompt* 1 2)
	    (ecase enter
	      (1 (go go-login-name))
	      (2 (go go-register-name))))
  (go-register-name rname (prompt-read "Введите имя: ")
		    (go go-register-pass1))
  (go-register-pass1 rpass1 (prompt-read "Введите пароль: ")
		     (go go-register-pass2))
  (go-register-pass2 rpass2 (prompt-read "Введите пароль ещё раз: ")
		     (if (string= rpass1 rpass2)
			 (if (user-exists-p rname)
			     (progn
			       (format t "Извините, пользователь с таким именем уже существует.~%")
			       (go go-register-name))
			     (progn 
			       (format t "Регистрация завершена.~%")
			       (register-user rname rpass1)
			       (go  go-enter)))
			 (progn (format t "Пароли не совпадают. Пожалуйста, пройдите регистрацию заново.~%")
				(go go-register-name))))
  (go-login-name lname (prompt-read "Имя: ")
		 (go go-login-pass))
  (go-login-pass lpass (prompt-read "Пароль: ")
		 (if (can-login lname lpass)
		     (login lname lpass)
		     (progn
		       (format t "Имя и пароль не соответствуют друг другу. Повторите ввод.~%")
		       (go go-login-name)))))
