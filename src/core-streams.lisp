(in-package :cl-user)
(defpackage :lispmud/core-streams
  (:use :cl)
  (:use :sb-gray)
  (:import-from :iter #:iter #:for #:with #:leave #:finally)
  (:import-from :alexandria #:last-elt))
(in-package :lispmud/core-streams)

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
   (buffer :initform (make-array 500 :element-type 'character :fill-pointer 0))
   (encoding :accessor encoding :initarg :encoding)))

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
  (with-slots (buffer columns inner-stream start-line-p encoding) stream
    (cond
      ((char= char #\Newline)
       (vector-push-extend #\Return buffer)
       (vector-push-extend #\Newline buffer)
       (send-string buffer inner-stream encoding)
       (force-output inner-stream)
       (setf (fill-pointer buffer) 0
	     start-line-p t))
      ((and (eq :cp1251 encoding) (char= char #\я))
       (vector-push-extend #\я buffer)
       (vector-push-extend #\я buffer)
       (setf start-line-p nil))
      (t
       (vector-push-extend char buffer)
       (setf start-line-p nil)))))

(defun send-string (buffer byte-stream encoding &key (start 0) end)
  (write-sequence (sb-ext:string-to-octets buffer :external-format encoding :start start :end end)
		  byte-stream)
  (force-output byte-stream))

(defmethod stream-write-string ((stream telnet-byte-output-stream) string &optional (start 0) end)
  (with-slots (buffer columns inner-stream start-line-p encoding) stream
    (iter (for i from start below (or end (length string)))
	  (for char = (aref string i))
	  (when (>= (1+ (fill-pointer buffer)) (array-dimension string 0))
	    (send-string buffer inner-stream encoding)
	    (setf (fill-pointer buffer) 0))
	  (case char
	    (#\я
	     (vector-push-extend #\я buffer)
             (when (eq :cp1251 encoding)
	       (vector-push-extend #\я buffer)))
	    (#\Newline
	     (vector-push-extend #\Return buffer)
	     (vector-push-extend #\Newline buffer))
	    (otherwise
	     (vector-push-extend char buffer))))
    (when (plusp (fill-pointer buffer))
      (send-string buffer inner-stream encoding)
      (setf (fill-pointer buffer) 0
	    start-line-p (char= #\Newline (last-elt string))))))
