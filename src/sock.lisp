(in-package :lispmud)

(defclass telnet-input-stream (fundamental-character-input-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod stream-element-type ((stream telnet-input-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream telnet-input-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defmethod stream-read-char ((stream telnet-input-stream))
  (let ((c (read-char (stream-of stream) nil :eof)))
    (if (char= c #\Return)
	(let ((c2 (read-char (stream-of stream) nil :eof)))
	  (if (char= c2 #\Newline)
	      #\Newline
	      (progn (unread-char c2 (stream-of stream))
		     #\Return)))
	c)))

(defmethod stream-unread-char ((stream telnet-input-stream)
			       char)
  (unread-char char (stream-of stream)))
     
(defmethod stream-read-line ((stream telnet-input-stream))
  (let ((str (read-line (stream-of stream))))
    (subseq str 0 (1- (length str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass telnet-output-stream (fundamental-character-output-stream)
  ((stream :initarg :stream :reader stream-of)
   (col-index :initform 0 :accessor col-index-of)))
     
(defmethod stream-element-type ((stream telnet-output-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream telnet-output-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defmethod stream-line-column ((stream telnet-output-stream))
  (col-index-of stream))

(defmethod stream-write-line ((stream telnet-output-stream) line)
  (with-accessors ((inner-stream stream-of)) stream
    (write line :stream inner-stream)
    (write-char #\Return inner-stream)
    (write-char #\Newline inner-stream)
    (force-output inner-stream)))


(defmethod stream-write-char ((stream telnet-output-stream)
			      char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)) stream
    (if (char= char #\Newline)
      (progn (write-char #\Return inner-stream)
	     (write-char char inner-stream)
	     (force-output inner-stream)
	     (setf cols 0))
      (progn (incf cols)
	     (write-char char inner-stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-telnet-stream (socket)
  (let ((raw-stream (socket-make-stream socket :input t :output t)))
    (make-two-way-stream
     (make-instance 'telnet-input-stream  :stream raw-stream)
     ;(make-string-output-stream)
     (make-instance 'telnet-output-stream :stream raw-stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-socket (addr port)
  "Quick'n'dirty temp function."
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address  s) T)
    (setf (sockopt-receive-buffer s) 0)
    (socket-bind s  addr port)
    (socket-listen s 5)
    (socket-accept s)))

(defun make-stream (addr port)
  "Quick'n'dirty temp function."
  (socket-make-stream (make-socket addr port)))

(defun telnet-read-line (stream)
  (let ((str (read-line stream)))
    (subseq str 0 (1- (length str)))))

(defun telnet-send-line (string stream)
  (write-line string stream)
  (force-output stream))

(defun start-telnet-server (addr port callback)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
	 (progn
	   (setf (sockopt-reuse-address  s) T)
	   (setf (sockopt-receive-buffer s) 0)
	   (socket-bind s  addr port)
	   (socket-listen s 5)
	   (loop
	      do (telnet-accept s callback)))
      (socket-close s))))

(defun telnet-accept (socket callback)
  (let* ((sock (socket-accept socket))
	 (sock-stream (socket-make-stream sock :input t :output t)))
    (unwind-protect
	 (sb-thread:make-thread #'(lambda () (funcall callback sock-stream)))
      (close sock-stream)
      (socket-close sock))))

;;; Example:
;;;
;;; (start-telnet-server #(0 0 0 0) 8002
;;;                      #'(lambda (stream) (write-line "Hello!" stream) (force-output stream) (close stream)))
