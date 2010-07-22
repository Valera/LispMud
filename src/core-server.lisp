;;; core-server.lisp
;;;
;;; Realization of thread pool tcp server. Made with tutorial by M. Bazon:
;;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(in-package :lispmud)

(defclass server ()
  ((cmdqueue-sem :initform (sb-thread:make-semaphore)) ; Семафор для работы с очередь команд.
   (cmdqueue :initform (sb-queue:make-queue)) ; Очередь команд. Каждая команда -- функция, которая будет вызвана свободной нитью.
   (workers-mutex :accessor server-workers-mutex :initform (bt:make-lock)) ; Блокировка для списка нитей-обработчиков данных.
   (workers :accessor server-workers :initform '()) ; Список нитей-обраточкиков.
   (connections-mutex :accessor server-connections-mutex :initform (bt:make-lock)) ; Блокировка на слот connections.
   (connections :accessor connections :initform (make-hash-table)) ; Хеш-таблица, отображающая сокеты на их клиентские объекты.
   (ctlqueue :accessor ctlqueue :initform (sb-queue:make-queue))))

(define-condition shutting-down () ()) ;; Not used now.
(define-condition disconnect-client () ())

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

(defgeneric handle-client-input (server socket)
  (:documentation "Called from event loop when data is available in socket")
  (:method ((server server) socket)
    (format t "handle-client-input ~s ~s~%" server socket)
    (with-slots (connections) server
      (let* ((client (gethash socket connections))
	     (input (client-read client socket)))
	(when input
	  (send-to-workers server (cons (curry #'client-on-command client socket input) socket)))))))

(defgeneric add-worker (server &optional n)
  (:documentation "Adds 'n' workers to thread pool.")
  (:method ((server server) &optional n)
    (bt:with-recursive-lock-held ((server-workers-mutex server))
      (dotimes (count n)
	(push (bt:make-thread #'(lambda () (worker-thread server))) (server-workers server))))))

(defgeneric worker-thread (server)
  (:documentation "Function that is executed within worker threads. Processes input queue.")
  (:method ((server server))
    (handler-case
        (with-slots (cmdqueue-sem cmdqueue) server
	  (iter
	    (iter (for event = (sb-queue:dequeue cmdqueue))
		  (while event)
		  (for (fun . socket) = event)
		  (handler-case ; А хорошо ли делать handler-case каждый раз?
		      (funcall fun)
		    (disconnect-client ()
		      (sb-queue:enqueue (cons 'disconnect-client socket) (ctlqueue server)))))
	    (sb-thread:wait-on-semaphore cmdqueue-sem)))
      (shutting-down ()
        ;; anything to do here?
	#+ nil (error "shitting-down handler-case not implemented"))
      (error (condition)
        (bt:with-recursive-lock-held ((server-workers-mutex server))
          (setf (server-workers server) (delete (bt:current-thread) (server-workers server)))
	  (format t "working-thread: error of type \"~A\", starting new thread"  condition)
	  (add-worker server 1))))))

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
  (:documentation "Main server loop, waits for input on 'master-socket' and dispatches data.")
  (:method ((server server) master-socket)
    (let ((sockets (list master-socket))
          (connlock (server-connections-mutex server)))
      (unwind-protect
	   (flet ((disconnect-socket (s)
		    (bt:with-lock-held (connlock)
		      (handle-client-disconnect server s))
		    (setf sockets (delete s sockets))
		    (socket-close s)))
	     (iter
	       (iter (for s in (wait-for-input sockets :ready-only t))

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
				 (disconnect-socket s)))
		       (end-of-file ()
			 ;; not sure we ever get here
			 ))
		     (unless (sb-queue:queue-empty-p (ctlqueue server))
		       ;; If queue is not emty, disconnet correcponging socket.
		       (destructuring-bind (message . socket)
			   (sb-queue:dequeue (ctlqueue server))
			 (format t "Got (~a ~a) in control queue" message socket)
			 (if (eq message 'disconnect-client)
			     (disconnect-socket socket)
			     (error "Unknow message ~S in control queue." message)))))))
	(bt:with-lock-held (connlock)
	  (iter (for s in sockets)
		(handle-client-disconnect server s)
		(socket-close s)))))))

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
   (globvars :accessor globvars)
   (player-state :accessor player-state :initform 'login)
   (register-and-login-fsm :accessor register-and-login-fsm)))

(defmethod initialize-instance :after ((client client) &key &allow-other-keys)
  (setf (out-stream client)
	(make-instance 'telnet-byte-output-stream :stream (socket-stream (socket client))))
  (setf (globvars client) (list '*standard-output* (out-stream client)
				'*player-zone* (first *zone-list*)
				'*player-room* (first (entry-rooms (first *zone-list*)))
				'*player* (make-instance 'player)))
  (write-line "Вы подключены к серверу LISPMUD." (out-stream client))
  (with-variables-from (globvars client)
      (*standard-output*)
    (setf (register-and-login-fsm client) (make-instance 'register-and-login-fsm))))

;; Temporary *out* string for setting it as *standard-output*
;(defparameter *out* (make-array 1000 :fill-pointer 0 :element-type 'character))

(defmethod client-on-command ((client client) socket input)
;  (with-output-to-string (stream *out*)
;    (format stream "client-on-command: ~a ~a ~s~%" client socket input))
  (with-variables-from (globvars client)
      (*standard-output* *player-zone* *player-room* *player*)
    (ecase (player-state client)
      (login
       (with-slots ((fsm register-and-login-fsm)) client
	 (process-input1 fsm (string-trim '(#\Space #\Newline #\Return) input))
	 (if (eql (current-state fsm) 'finish-login)
	     (progn
	       (setf (player-state client) 'game)
	       (room-about *player-room*)))))
	 ;; FIXME: Выход без регистрации.
      (game
       (handler-case
	   (progn (exec-command (string-trim '(#\Space #\Newline #\Return) input))
		  (room-about *player-room*))
	 (error (condition) (format t "Command erred with condition ~a~%" condition)))))))

;  (format (out-stream client) "Echo: ~a~%" input))

(defun collect-input (socket buffer &optional (end-byte 10))
  (loop
     :with stream = (socket-stream socket)
     :with byte
     :with prev-byte = -1
     :while (listen stream)
     :doing
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
