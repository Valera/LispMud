;;; command.lisp

(in-package :lispmud)

;(defvar *command-hash* (make-hash-table :test 'equal))

(defclass node ()
  ((string :accessor node-string :initarg :string :initform "")
   (value :accessor node-value :initarg :value)
   (left :accessor node-left :initform nil)
   (right :accessor node-right :initform nil)))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a :left ~a :right ~a"
	    (node-string object) (node-left object) (node-right object))))

(defvar *command-root* (make-instance 'node)
  "Root node of command binary tree")

;(defun add-command (string function)
;  (setf (gethash string *command-hash*) function))

(defun add-command (string fun)
  (add-command3 *command-root* string fun))

(defun add-command3 (tree string fun)
  (if (or (string= string (node-string tree)) (= 0 (length (node-string tree))))
      (setf (node-string tree) string)
      (if (string< string (node-string tree))
	  (if (node-left tree)
	      (add-command3 (node-left tree) string fun)
	      (setf (node-left tree) (make-instance 'node :string string)))
	  (if (node-right tree)
	      (add-command3 (node-right tree) string fun)
	      (setf (node-right tree) (make-instance 'node :string string))))))

(defun find-closest-command (tree string)
  (if (<= (length string) 1)
      nil
      (find-closest-command1 tree string)))

(defun find-closest-command1 (tree string)
  (if (string= string (node-string tree))
      (node-value tree)
      (if (string< string (node-string tree))
	  (if (node-left tree)
	      (find-closest-command1 tree string)
	      (final-check tree string))
	  (if (node-right tree)
	      (find-closest-command1 tree string)
	      (final-check tree string)))))

(defun final-check (tree str)
  "Final test, does str fit for string slot of node. 
  For ex. can be called with node value \"take\" and string \"takeeeeee\".
  That is wrong, function returns nil."
  (with-accessors ((s node-string)) tree
    (if (= (length str) (or (string< str s) (string>= str s)))
	(node-value tree)
	nil)))

(defun init-command-hash ()
  (add-command "эхо"  #'(lambda (&rest args) (format t "~{|~a| ~}~%" args)))
  (add-command "s" nil)
  (add-command "n" nil)
  (add-command "w" nil)
  (add-command "e" nil)
  (add-command "exit" nil))

(defun parse-command (command-string)
  (split-sequence #\Space command-string  :remove-empty-subseqs t))

(defun exec-command (command-string)
  (let* ((command-and-args (parse-command command-string))
	 (command (first command-and-args))
	 (args (rest command-and-args))
	 (command-fun (find-closest-command *command-root* command)))
    (if command-fun
	(apply command-fun args)
	(format t "Комманда не найдена. Первая комманда ~a ~a~%" *command-root* (node-string *command-root*)))))
