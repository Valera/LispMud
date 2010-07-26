(in-package :lispmud)

(defun all-positions (elem sequence &key (start 0) end)
  (iter (with positions)
	(with cur-pos)
	(with index = start)
	(if (setf cur-pos (position elem sequence :start index :end end))
	    (push cur-pos positions)
	    (leave (nreverse positions)))
	(setf index (1+ cur-pos))))

(defun neigbours-list (list)
  (iter (with prev = (first list))
	(for elem in (rest list))
	(with result)
	(push (cons prev elem) result)
	(setf prev elem)
	(finally (return (nreverse result)))))

(defclass telnet-byte-output-stream (fundamental-character-output-stream)
  ((inner-stream :initarg :stream :reader inner-stream)
   (col-index :initform 0 :accessor col-index-of)
   (start-line-p :initform t :accessor start-line-p)
   (buffer :initform (make-array 200 :element-type 'character :fill-pointer 0))))
     
(defmethod stream-element-type ((stream telnet-byte-output-stream))
  'character)

(defmethod close ((stream telnet-byte-output-stream) &key abort)
  (close (inner-stream stream) :abort abort))

#+nil
(defmethod stream-line-column ((stream telnet-byte-output-stream))
  (col-index-of stream))

(defmethod stream-line-length ((stream telnet-byte-output-stream))
  nil)

(defmethod stream-start-line-p ((stream telnet-byte-output-stream))
  (start-line-p stream))

(defmethod stream-force-output ((stream telnet-byte-output-stream))
  (force-output (inner-stream stream)))

(defmethod stream-write-char ((stream telnet-byte-output-stream) char)
  (with-slots (buffer columns inner-stream start-line-p) stream
    (cond
      ((char= char #\Newline)
       (vector-push-extend #\Return buffer)
       (vector-push-extend #\Newline buffer)
       (stream-write-string stream buffer)
       (force-output inner-stream)
       (setf (fill-pointer buffer) 0
	     start-line-p t))
      (t
       (vector-push-extend char buffer)
       (setf start-line-p nil)))))

(defun send-string (buffer byte-stream &key start end)
;  (format t "sending stream ~s~%" buffer)
  (write-sequence (string-to-octets buffer :external-format :cp1251 :start start :end end)
		  byte-stream)
  (force-output byte-stream))

;stream-write-string stream string &optional start end
(defvar *ya-bytes* (string-to-octets "яя" :external-format :cp1251))

;#+nil
(defmethod stream-write-string ((stream telnet-byte-output-stream) string &optional (start 0) end)
  (with-slots (buffer columns inner-stream) stream
    (let ((positions (all-positions #\я string :start start :end end)))
      (if positions
	  ;; then: печать кусочков строки, печатая вместо буквы "я" "яя".
	  ;; Хак для правильной обработки телнетом этой буквы.
	  (let ((otrezki  (neigbours-list (append `(,start) positions `(,end)))))
	    (iter (for (start . end) in otrezki)
		  (if-first-time
		   (progn)
		   (progn (write-sequence *ya-bytes* inner-stream)
			  (incf start)))
		  (send-string string inner-stream :start start :end end)))
	  ;; else: букв я в строке нет, посылаем её целиком.
	  (send-string string inner-stream :start start :end end))))
  (setf (start-line-p stream) (char= (last-elt string) #\Newline)))

