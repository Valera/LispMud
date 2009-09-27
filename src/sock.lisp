(in-package :lispmud)

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
