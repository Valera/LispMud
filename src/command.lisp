;;; command.lisp

(in-package :lispmud)

(defvar *command-hash* (make-hash-table :test 'equal))

(defun command-go-to-direction (direction)
  (assert (member direction '(:south :north :west :east)))
  (let ((next-room (gethash (cons (cur-room *thread-vars*) direction)
			    (exit-hash (cur-zone *thread-vars*)))))
    (if next-room
	(setf (cur-room *thread-vars*) next-room)
	(format t "Извините, но вы не можете идти в этом направлении"))))

(defun command-leave ()
  (format t "До свидания, возвращайтесь быстрей!~%")
  (setf (end-p *thread-vars*) t))
  

(defun init-command-table ()
  (let ((command-list nil)
	(command-hash (make-hash-table :test 'equal)))
    (flet ((add-command (string fun) (push (list string fun) command-list)))
      (add-command "эхо"  #'(lambda (&rest args) (format t "~{|~a| ~}~%" args)))
      (add-command "ю" #'(lambda () (command-go-to-direction :south)))
      (add-command "с" #'(lambda () (command-go-to-direction :north)))
      (add-command "з" #'(lambda () (command-go-to-direction :west)))
      (add-command "в" #'(lambda () (command-go-to-direction :east)))
      (add-command "конец" #'command-leave))
    (setf command-list (nreverse command-list))
    (dolist (i command-list)
      (let ((i-str (first i))
	    (i-fun (second i)))
	(dotimes (j (length i-str))
	  (let ((substr (subseq i-str 0 (1+ j))))
	    (unless (gethash substr command-hash)
		   (setf (gethash substr command-hash) i-fun))))))
    (setf *command-hash* command-hash)))

(defun parse-command (command-string)
  (split-sequence #\Space command-string  :remove-empty-subseqs t))

(defun exec-command (command-string)
  (let* ((command-and-args (parse-command command-string))
	 (command (first command-and-args))
	 (args (rest command-and-args))
	 (command-fun (gethash command *command-hash*)))
    (if command-fun
	(apply command-fun args)
	(format t "Комманда не найдена.~%"))))
