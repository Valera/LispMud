(in-package :lispmud)

(generate-fsm text-editor-fsm editing finish-editing
    ((text :accessor text)
     (buffer-stream :accessor buffer-stream :initform (make-string-output-stream)))
  (editing
   :on-enter (format t "=== Отредактируйте текст: ===")
   :on-input (progn (if (string= input "\\s")
			(next-state finish-editing))
		    (write-string input (buffer-stream fsm))
		    (color *cc-green*)
		    (write-string input)
		    (color *cc-green*)))
  (finish-editing
   :on-enter (progn (format t "=== Конец текста. ===")
		    (setf (text fsm) (get-output-stream-string (buffer-stream fsm)))
	#+nil	    (pop-input-hander))))
   