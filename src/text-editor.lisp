(in-package :cl-user)
(defpackage :lispmud/text-editor
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:import-from :lispmud/color-codes #:*cc-green* #:color #:*cc-reset*)
  (:import-from :lispmud/core-zone #:zone)
  (:import-from :lispmud/core-fsm #:generate-fsm #:next-state #:fsm #:input #:process-input1))
(in-package :lispmud/text-editor)

(generate-fsm text-editor-fsm editing finish-editing
    ((text :accessor text)
     (buffer-stream :accessor buffer-stream :initform (make-string-output-stream))
     (after-editing-cb :accessor after-editing-cb :initform nil :initarg :after-editing-cb))
  (editing
   :on-enter
   (format t "=== Напишите текст. Чтобы закончить писать, на новой строке введите \\s: ===~%")
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
		      (funcall (after-editing-cb fsm) (text fsm))))))

