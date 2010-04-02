;;; core-server.lisp

(in-package :lispmud)

#+nil
(defun listener-loop (host port)
  (let* ((master-socket
	  (usocket:socket-listen host port
				 :reuse-address t
				 :element-type 'unsigned-byte))
	 (sockets (list master-socket)))
    ;; see below for the :ready-only description
    (iter
     ;; infinite loop
     (iter
      (for s in (wait-for-input sockets :ready-only t))
      (if (eq s master-socket)
	  ;; THEN: new connection
	  (let ((new (usocket:socket-accept s)))
	    ;; add the new socket to the list of sockets that we're listening to
	    (setf sockets (nconc sockets `(,new)))
	    (handle-client-connect new))
	  ;; ELSE: data from a connected client
	  (handle-client-input s))))))
;(defparameter *serv*
;	   (make-instance 'server))
;(defvar *master-socket*
;	   (socket-listen "localhost" 3001 :element-type 'unsigned-byte))
;(provider-thread *serv* *master-socket*)

(defparameter *out* (make-array 1000 :fill-pointer 0 :element-type 'character))

(defun client-on-command (client input)
  (with-output-to-string (stream *out*)
    (format stream "client-on-command: ~a ~s~%" client input)))

(defun handle-client-connect (server new)
  (format t "connect: ~a ~a~%" server new)
  (setf (gethash new (connections server)) (make-instance 'client)))

(defun handle-client-disconnect (server s)
  (format t "disconnect: ~a ~a~%" server s))

(defclass client ()
  ((buffer :accessor client-buffer :initform (make-array 200 :fill-pointer 0 :element-type '(unsigned-byte 8)))))

(defclass server ()
  ((cmdqueue-sem :initform (sb-thread:make-semaphore))
   (cmdqueue :initform (sb-queue:make-queue))
   (workers-mutex :accessor server-workers-mutex :initform (bt:make-lock))
   (workers :accessor server-workers :initform '())
   (connections-mutex :accessor server-connections-mutex :initform (bt:make-lock))
   (connections :accessor connections :initform (make-hash-table))))

(defun add-worker (server)
  (bt:with-lock-held ((server-workers-mutex server))
    (push (bt:make-thread #'(lambda () (worker-thread server))) (server-workers server))))

(defun collect-input (socket buffer &optional (end-char 13))
  (loop
     :with stream = (socket-stream socket)
     :with byte
     :while (listen stream)
     :doing
     (format t "collect-input: listened")
     ;(format (socket-stream socket) "Hello there~%")   ;; output into buffers
     ;(force-output (socket-stream socket))
     ;(print (peek-char nil (socket-stream socket)))
     (setq byte (read-byte stream))
     (format t "collect-input: read-byte ~a~%" byte)
     (when (= byte end-char)
       (return t))
     (vector-push-extend byte buffer)))

(defun reset-buffer (client)
  (setf (fill-pointer (client-buffer client)) 0))

(defgeneric client-read (client socket)
  (:method ((client client) socket)
    (format t "client-read ~s ~s~%" client socket)
    (with-slots (buffer) client
      (when (collect-input socket buffer)
        (prog1
            (octets-to-string buffer); :external-format :cp1251)
          (reset-buffer client))))))

(defgeneric handle-client-input (server socket)
  (:method ((server server) socket)
    (format t "handle-client-input ~s ~s~%" server socket)
    (with-slots (connections) server
      (let* ((client (gethash socket connections))
	     (input (client-read client socket)))
	(send-to-workers server (curry #'client-on-command client input))))))

;;(defun curry (fun &rest args1)
;;  (lambda (&rest args2)
;;    (apply fun (append args1 args2))))

(defgeneric worker-thread (server)
  (:method ((server server))
    (handler-case
        (with-slots (cmdqueue-sem cmdqueue) server
          (loop
             (loop :for event = (sb-queue:dequeue cmdqueue)
                :while event
                :do (funcall event))
             (sb-thread:wait-on-semaphore cmdqueue-sem)))
      (shutting-down ()
        ;; anything to do here?
        )
      (error (condition)
        (bt:with-lock-held ((server-workers-mutex server))
          (delete (bt:current-thread) (server-workers server)))
        (format t "~A"  condition)
        ;; XXX: should start another worker here, or we'll run out
        ))))

(defgeneric send-to-workers (server event)
  (:method ((server server) event)
    (with-slots (cmdqueue-sem cmdqueue) server
      (sb-queue:enqueue event cmdqueue)
      (sb-thread:signal-semaphore cmdqueue-sem))))


;;(bt:interrupt-thread *listener-thread*
;;  #'(lambda () (signal 'shutting-down)))
;;(bt:join-thread *listener-thread*)

(defgeneric provider-thread (server master-socket)
  (:method ((server server) master-socket)
    (let ((sockets (list master-socket))
          (connlock (server-connections-mutex server)))
      (handler-case
          (loop
             (loop :for s :in (wait-for-input sockets :ready-only t) :doing
                (handler-case
                    (if (eq s master-socket)
                        ;; THEN: we have new connection
                        (progn
                          (bt:with-lock-held (connlock)
                            (unless (null (slot-value s 'usocket::state))
                              (let ((new (socket-accept s)))
                                (setf sockets (push new sockets))
                                (handle-client-connect server new)))))
                        ;; ELSE: client socket
                        (if (listen (socket-stream s))
                            ;; THEN: input available
                            (handle-client-input server s)
                            ;; ELSE: EOF, lost connection
                            (progn
                              (bt:with-lock-held (connlock)
                                (handle-client-disconnect server s))
                              (setf sockets (delete s sockets))
                              (socket-close s))))
                  (end-of-file ()
                    ;; not sure we ever get here
                    ))))
        (shutting-down ()
          ;; anything to do here?
          )))))
