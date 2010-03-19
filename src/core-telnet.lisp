(in-package :lispmud)

(defclass telnet-input-stream (fundamental-character-input-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod stream-element-type ((stream telnet-input-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream telnet-input-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defmethod stream-line-length ((stream telnet-input-stream))
  nil)

(defmethod stream-line-column ((stream telnet-input-stream))
  "Don't know what line column on the client side, returning 0"
  '0)

(defmethod stream-read-char ((stream telnet-input-stream))
  (let ((c (read-char (stream-of stream) nil :eof)))
    (if (eq c :eof)
	:eof
	(if (char= c #\Return)
	    (let ((c2 (read-char (stream-of stream) nil :eof)))
	      (if (char= c2 #\Newline)
	      #\Newline
	      (progn (unread-char c2 (stream-of stream))
		     #\Return)))
	    c))))

(defmethod stream-read-char-no-hang ((stream telnet-input-stream))
  (let ((c (read-char-no-hang (stream-of stream) nil :eof)))
    (when c
      (if (eq c :eof)
	  :eof
	  (if (char= c #\Return)
	      (let ((c2 (read-char (stream-of stream) nil :eof)))
		(if (char= c2 #\Newline)
		    #\Newline
		    (progn (unread-char c2 (stream-of stream))
			   #\Return)))
	      c)))))


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

(defmethod stream-line-length ((stream telnet-output-stream))
  nil)

(defmethod stream-force-output ((stream telnet-output-stream))
  (force-output (stream-of stream)))

(defmethod stream-write-char ((stream telnet-output-stream)
			      char)
  (with-accessors ((inner-stream stream-of) (cols col-index-of)) stream
    (if (char= char #\Newline)
      (progn (write-char #\Return inner-stream)
	     (write-char #\Newline inner-stream)
	     (force-output inner-stream)
	     (setf cols 0))
      (progn (incf cols)
	     (write-char char inner-stream)
             ;; FIXME работа с буквой "Я".
	     (when (member char '(#\я #\Я))
	       (incf cols)
	        (write-char char inner-stream))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-telnet-stream (socket)
  (let ((raw-stream (socket-make-stream socket :input t :output t :buffering :line :external-format :cp1251)))
    (make-two-way-stream
     (make-instance 'telnet-input-stream  :stream raw-stream)
     (make-instance 'telnet-output-stream :stream raw-stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-server-socket (addr port)
  "Launch telnet listener and return only one accepted socket for debug purpose."
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address  s) T)
    (socket-bind s  addr port)
    (socket-listen s 5)
    (socket-accept s)))

(defun start-telnet-server (addr port callback)
  "Starts telnet server for given address and port.
  Launches callback with every accepted telnet stream as argument."
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
	 (progn
	   (setf (sockopt-reuse-address  s) T)
	   (socket-bind s  addr port)
	   (socket-listen s 5)
	   (loop
	      do (telnet-accept s callback)))
      (socket-close s))))

(defun telnet-accept (socket callback)
  "Fucntion for accepting connections. Accepts and creates telnet stream."
  (let ((sock (socket-accept socket)))
    (make-thread
     (lambda ()
       (unwind-protect
	    (let ((stream (make-telnet-stream sock)))
	      (unwind-protect
		   (funcall callback stream)
		(close stream)))
	 (socket-close sock))))))

;;; Example:
;;;
;;; (start-telnet-server #(0 0 0 0) 8002
;;;                      #'(lambda (stream) (write-line "Hello!" stream) (force-output stream) (close stream)))
