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
       (send-string buffer inner-stream)
       (force-output inner-stream)
       (setf (fill-pointer buffer) 0
	     start-line-p t))
      ((char= char #\я)
       (vector-push-extend #\я buffer)
       (vector-push-extend #\я buffer)
       (setf start-line-p nil))
      (t
       (vector-push-extend char buffer)
       (setf start-line-p nil)))))

(defun send-string (buffer byte-stream &key (start 0) end)
  (write-sequence (string-to-octets buffer :external-format :cp1251 :start start :end end)
		  byte-stream)
  (force-output byte-stream))

(defvar *ya-bytes* (string-to-octets "яя" :external-format :cp1251))

(defmethod stream-write-string ((stream telnet-byte-output-stream) string &optional (start 0) end)
  (flet ((substring-length (string start end)
	   "Return lenght of sting from start to end. End can be nil."
	   (- (or end (length string))
	      start)))
    (with-slots (buffer columns inner-stream start-line-p) stream
      (let ((positions (all-positions #\я string :start start :end end)))
	(if positions
	    ;; then: печать кусочков строки, печатая вместо буквы "я" "яя".
	    ;; Хак для правильной обработки телнетом этой буквы.
	    (let ((otrezki  (neigbours-list (append `(,start) positions `(,end)))))
	      (iter (for (start . end) in otrezki)
		    (unless (first-time-p)
		      (vector-push-extend #\я buffer)
		      (vector-push-extend #\я buffer)
		      (incf start))
		    (for len = (substring-length string start end))
		    (incf (fill-pointer buffer) len)
		    (replace buffer string :start1 (- (fill-pointer buffer) len)
			     :start2 start :end2 end)
		    (send-string buffer inner-stream)))
	    ;; else: букв я в строке нет, посылаем её целиком.
	    (if (= 0 (fill-pointer buffer))
		(send-string string inner-stream :start start :end end)
		(let ((len (substring-length string start end)))
		  (incf (fill-pointer buffer) len)
		  (replace buffer string :start1 (- (fill-pointer buffer) len))
		  (send-string buffer inner-stream)))))
      (setf (fill-pointer buffer) 0)
      (setf start-line-p (char= (last-elt string) #\Newline)))))

