;;; command.lisp

(in-package :lispmud)

(defvar *command-hash* (make-hash-table :test 'equal)
  "Хэш-таблица, ставит в соответсвие строке команды функцию для её обработки")

(defun init-command-table (commands-spec)
  (let 	((command-hash (make-hash-table :test 'equal)))
    (iter (for (command-name command-fun) in commands-spec)
	  (dotimes (j (length command-name ))
	    (let ((substr (subseq command-name  0 (1+ j))))
	      (unless (gethash substr command-hash)
		(setf (gethash substr command-hash) command-fun)))))
    (setf *command-hash* command-hash)))

(5am:def-suite command
    :description "command test suite")

(5am:in-suite command)

(5am:def-fixture setup-command-hash (commands-spec)
  (let ((*command-hash* (make-hash-table :test 'equal)))
    (init-command-table commands-spec)
    (&body)))

(5am:test init-command-table
  (5am:with-fixture setup-command-hash ('(("abcd" #'id)))
    (5am:is (set-equal (hash-table-keys *command-hash*)
                       '("a" "ab" "abc" "abcd")
                       :test #'string=))))

(defun exec-command (command-string)
  (let ((command-and-args (split-sequence #\Space command-string  :remove-empty-subseqs t)))
    (if command-and-args
	(destructuring-bind (command &rest args) command-and-args
	  (let ((command-fun (gethash command *command-hash*)))
	    (if command-fun
		(apply command-fun args)
		(format t "Комманда не найдена.~%"))))
	(format t "Ээээ... что?~%"))))

(5am:test exec-command
  (let ((last-command))
    (labels
        ((f (&rest data) (setf last-command `(f ,data)))
         (g (&rest data) (setf last-command `(g ,data)))
         (run-test (command)
           (setf last-command nil)
           (exec-command command)
           last-command))
      (5am:with-fixture setup-command-hash (`(("abc" ,#'f) ("abD" ,#'g)))
        (5am:is (equal (run-test "abc a b c") '(f ("a" "b" "c"))))
        (5am:is (equal (run-test "ab a b c") '(f ("a" "b" "c"))))
        (5am:is (equal (run-test "abD a") '(g ("a"))))
        (5am:is (eq (run-test "hello world") nil))
        (5am:is (eq (run-test "") nil))))))

(defmacro word-dispatch (word &body clauses)
  `(when (plusp (length ,word))
     (cond
       ,@(iter (for (case-word . forms) in clauses)
	       (collecting
		 (if (not (eql case-word t))
		     (let ((mismatch-gensym (gensym)))
                       `((let ((,mismatch-gensym (mismatch ,word ,case-word)))
                          (or (not ,mismatch-gensym) (= ,mismatch-gensym (length ,word))))
                         ,@forms))
		     `(t ,@forms)))))))

(5am:test word-dispatch
  (let ((x))
    (labels
        ((test-word (word)
           (setf x nil)
           (word-dispatch word
             ("abc" (setf x "abc"))
             ("abD" (setf x "abD"))
             (t (setf x "default")))
           x))
      (5am:is (equal "abc" (test-word "abc")))
      (5am:is (equal "abc" (test-word "ab")))
      (5am:is (equal "abD" (test-word "abD")))
      (5am:is (equal nil (test-word "")))
      (5am:is (equal "default" (test-word "xyz"))))))
