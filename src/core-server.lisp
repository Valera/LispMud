;;; core-server.lisp

(in-package :lispmud)

(defclass server ()
  ((cmdqueue-sem :initform (sb-thread:make-semaphore))
   (cmdqueue :initform (sb-queue:make-queue))
   (workers-mutex :accessor server-workers-mutex :initform (bt:make-lock))
   (workers :accessor server-workers :initform '())
   (connections-mutex :accessor server-connections-mutex :initform (bt:make-lock))
   (connections :accessor connections :initform (make-hash-table))))

;; Declarations of client class interface

(defgeneric client-on-command (client socket input)
  (:documentation "Curried with data read with client read and enqueued in thread pool queue."))
(defgeneric client-read (client socket)
  (:documentation "Called when data is available in socket"))

;; Server implementation.

(defgeneric handle-client-connect (server new-socket)
  (:documentation "Called with new new connection 'new-socket'. Creates client.")
  (:method ((server server) new-socket)
    (format t "connect: ~a ~a~%" server new-socket)
    (setf (gethash new-socket (connections server)) (make-instance 'client :socket new-socket))))

(defgeneric handle-client-disconnect (server socket)
  (:documentation "Called on client disconnect, removes client from client hash.")
  (:method ((server server) socket)
    (remhash socket (connections server))
    (format t "disconnect: ~a ~a~%" server socket)
    (socket-close socket)))

(defgeneric add-worker (server &optional n)
  (:documentation "Adds 'n' workers to thread pool.")
  (:method ((server server) &optional n)
    (bt:with-lock-held ((server-workers-mutex server))
      (dotimes (count n)
	(push (bt:make-thread #'(lambda () (worker-thread server))) (server-workers server))))))

(defgeneric handle-client-input (server socket)
  (:documentation "Called from event loop when data is available in socket")
  (:method ((server server) socket)
    (format t "handle-client-input ~s ~s~%" server socket)
    (with-slots (connections) server
      (let* ((client (gethash socket connections))
	     (input (client-read client socket)))
	(when input
	  (send-to-workers server (curry #'client-on-command client socket input)))))))

(defgeneric worker-thread (server)
  (:documentation "Working function for launching new thread. Processes input queue.")
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
  (:documentation "Enqueues event to server queue.")
  (:method ((server server) event)
    (with-slots (cmdqueue-sem cmdqueue) server
      (sb-queue:enqueue event cmdqueue)
      (sb-thread:signal-semaphore cmdqueue-sem))))

;;(bt:interrupt-thread *listener-thread*
;;  #'(lambda () (signal 'shutting-down)))
;;(bt:join-thread *listener-thread*)

(defgeneric provider-thread (server master-socket)
  (:documentation "Main server loop, waits for input on 'master-sockets' and dispatches data.")
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
                              (let ((new (socket-accept s :element-type '(unsigned-byte 8))))
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
	#+nil (shutting-down ()
		;; anything to do here?
		)))))

(defun run-lispmud (port &key (host "localhost") (n-thread 1))
  (let ((serv (make-instance 'server))
	(master-socket (socket-listen host port :element-type 'unsigned-byte)))
    (unwind-protect
	 (progn
	   (add-worker serv n-thread)
	   (provider-thread serv master-socket))
      (socket-close master-socket))))


;; ================== Basic client implementation ==================

;; Client class.
(defclass client ()
  ((socket :accessor socket :initarg :socket)
   (out-stream :accessor out-stream)
   (buffer :accessor client-buffer :initform (make-array 200 :fill-pointer 0 :element-type '(unsigned-byte 8)))
   (globvars :accessor globvars)))

(defmethod initialize-instance :after ((client client) &key &allow-other-keys)
  (setf (out-stream client)
	(make-instance 'telnet-byte-output-stream :stream (socket-stream (socket client))))
  (setf (globvars client) (list '*standard-output* (out-stream client)
				'*player-zone* (first *zone-list*)
				'*player-room* (first (entry-rooms (first *zone-list*)))))
  (write-line "Привет, мир!" (out-stream client))
  (write-line "Привет, я сервер!" (out-stream client)))

;; Temporary *out* string for setting it as *standard-output*
;(defparameter *out* (make-array 1000 :fill-pointer 0 :element-type 'character))

(defmethod client-on-command ((client client) socket input)
;  (with-output-to-string (stream *out*)
;    (format stream "client-on-command: ~a ~a ~s~%" client socket input))
  (with-variables-from (globvars client)
      (*standard-output* *player-zone* *player-room*)
    (exec-command (string-trim '(#\Space #\Newline #\Return) input))
    (room-about *player-room*)
    (prompt)))


;  (format (out-stream client) "Echo: ~a~%" input))

(defun collect-input (socket buffer &optional (end-byte 10))
  (loop
     :with stream = (socket-stream socket)
     :with byte
     :with prev-byte = -1
     :while (listen stream)
     :doing
;     (format t "collect-input: listened; ")
     ;(format (socket-stream socket) "Hello there~%")   ;; output into buffers
     ;(force-output (socket-stream socket))
     ;(print (peek-char nil (socket-stream socket)))
     (setq byte (read-byte stream))
;     (format t "collect-input: read-byte ~s~%" byte)
     (when (= byte end-byte)
       (return t))
     (when (not (and (= byte 255) (= byte prev-byte)))
       (vector-push-extend byte buffer))
     (setf prev-byte byte)))

(defun reset-buffer (client)
  (setf (fill-pointer (client-buffer client)) 0))

(defmethod client-read ((client client) socket)
;  (format t "client-read ~s ~s~%" client socket)
  (with-slots (buffer) client
    (when (collect-input socket buffer)
      (prog1
	  (octets-to-string buffer :external-format :cp1251)
	(reset-buffer client)))))
