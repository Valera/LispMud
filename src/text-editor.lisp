(in-package :lispmud)

(generate-fsm text-editor-fsm editing finish-editing
    ((text :accessor text)
     (buffer-stream :accessor buffer-stream :initform (make-string-output-stream))
     (after-editing-cb :accessor after-editing-cb :initform nil :initarg :after-editing-cb))
  (editing
   :on-enter (format t "=== Отредактируйте текст: ===~%")
   :on-input (progn (if (string= (string-trim '(#\Space #\Newline #\Return) input) "\\s")
			(next-state finish-editing))
		    (write-string input (buffer-stream fsm))
		    (color *cc-green*)
		    (write-string input)
		    (terpri)
		    (color *cc-reset*)))
  (finish-editing
   :on-enter (progn (format t "=== Конец текста. ===")
		    (setf (text fsm) (get-output-stream-string (buffer-stream fsm)))
		    (when (after-editing-cb fsm)
		      (funcall (after-editing-cb fsm) (text fsm)))
		    (pop-input-handler))))
   