;;; core-server.lisp
;;;
;;; Realization of thread pool tcp server. Made with tutorial by M. Bazon:
;;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(in-package :cl-user)
(defpackage :lispmud/core-server
  (:use :cl)
  (:use :usocket)
  (:use :lispmud/core-globalvars)
  (:use :lispmud/core-threadvars)
  (:import-from :alexandria #:curry #:deletef)
  (:import-from :iter #:iter #:for)
  (:import-from :lispmud/core-room #:players)
  (:import-from :lispmud/userdb #:set-user-offline)
  (:import-from :lispmud/core-zone #:entry-rooms)
  (:import-from :lispmud/registration #:register-and-login-fsm)
  (:import-from :lispmud/core-streams #:telnet-byte-output-stream)
  (:import-from :lispmud/core-utils #:with-variables-from #:pvalue))
(in-package :lispmud/core-server)

(defclass server ()
  ((cmd-mailbox :initform (sb-concurrency:make-mailbox)) ; Очередь команд. Каждая команда -- функция, которая будет вызвана свободной нитью.
   (workers-mutex :accessor server-workers-mutex :initform (bt:make-lock)) ; Блокировка для списка нитей-обработчиков данных.
   (workers :accessor server-workers :initform '()) ; Список нитей-обраточкиков.
   (connections-mutex :accessor server-connections-mutex :initform (bt:make-lock)) ; Блокировка на слот connections.
   (connections :accessor connections :initform (make-hash-table)) ; Хеш-таблица, отображающая сокеты на их клиентские объекты.
   (ctlqueue :accessor ctlqueue :initform (sb-queue:make-queue)) ; Очередь служебных сообщений на закрытие сокетов от вокер-тредов к провайдер-треду
   ))

(define-condition shutting-down () ()) ;; Not used now.
(define-condition disconnect-client () ())

;; Declarations of client class interface

(defgeneric client-on-command (client socket input)
  (:documentation "Curried with data read with client read and enqueued in thread pool queue."))
(defgeneric client-read (client socket)
  (:documentation "Called when data is available in socket"))
(defgeneric client-disconnect (client)
  (:documentation "Called when client have disconected."))
(defgeneric first-client-interaction (client)
  (:documentation "Called right after connection, intended for greeting and login"))

;; Server implementation.

(defgeneric handle-client-connect (server new-socket)
  (:documentation "Called with new new connection 'new-socket'. Creates client.")
  (:method ((server server) new-socket)
    (format t "connect: ~a ~a~%" server new-socket)
    (let* ((encoding :utf8)            ; TODO: add support for changing encoding
           (client-out-stream
             (make-instance 'telnet-byte-output-stream
                            :stream (socket-stream new-socket) :encoding encoding))
           (new-client (make-instance 'client :socket new-socket :encoding encoding
                                              :out-stream client-out-stream)))
      (first-client-interaction new-client)
      (setf (gethash new-socket (connections server)) new-client))))

(defgeneric handle-client-disconnect (server socket)
  (:documentation "Called on client disconnect, removes client from client hash.")
  (:method ((server server) socket)
    (client-disconnect (gethash socket (connections server)))
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
    (unwind-protect
	 (handler-case
	     (with-slots (cmd-mailbox) server
               (iter (for event = (sb-concurrency:receive-message cmd-mailbox))
	         (for (fun . socket) = event)
		 (handler-case
		     (funcall fun)
		   (disconnect-client ()
		     (sb-queue:enqueue (cons 'disconnect-client socket) (ctlqueue server))))))
	   (shutting-down ()
	     ;; anything to do here?
	     (error "shutting-down handler-case not implemented"))
	   (sb-int:closed-stream-error ()
	     ;; FIXME: это случается когда provider-thread ещё не успела удалить
	     ;; сокет и ему пришла новая команда на обработку. В принципе, этой ошибки быть не должно.
	     ))
      (bt:with-recursive-lock-held ((server-workers-mutex server))
	(setf (server-workers server) (delete (bt:current-thread) (server-workers server)))
	(add-worker server 1)))))

(defgeneric send-to-workers (server event)
  (:documentation "Enqueues event to server queue.")
  (:method ((server server) event)
    (with-slots (cmd-mailbox) server
      (sb-concurrency:send-message cmd-mailbox event))))

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
			 (format t "Got (~a ~a) in control queue~%" message socket)
			 (if (eq message 'disconnect-client)
			     (disconnect-socket socket)
			     (error "Unknow message ~S in control queue." message)))))))
	(bt:with-lock-held (connlock)
	  (iter (for s in sockets)
		(unless (eql s master-socket)
		  (handle-client-disconnect server s))
		(socket-close s)))))))

(defun run-lispmud (port &key (host #(0 0 0 0)) (n-thread 1))
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
   (out-stream :accessor out-stream :initarg :out-stream)
   (buffer :accessor client-buffer :initform (make-array 200 :fill-pointer 0 :element-type '(unsigned-byte 8)))
   (encoding :accessor encoding :initarg :encoding)  ; used for decoding input
   (globvars :accessor globvars)
   (player-state :accessor player-state :initform :login)
   (input-handlers :accessor input-handlers :initform nil)
   (register-and-login-fsm :accessor register-and-login-fsm)))

(defvar *client*)

(defmethod initialize-instance :after ((client client) &key &allow-other-keys)
  (setf (globvars client) (list '*standard-output* (out-stream client)
				'*player-zone* (first *zone-list*)
				'*player-room* (first (entry-rooms (first *zone-list*)))
				'*player* nil
				'*client* client)))

;; Temporary *out* string for setting it as *standard-output*
;(defparameter *out* (make-array 1000 :fill-pointer 0 :element-type 'character))

(defun push-input-handler (handler)
  (push handler (input-handlers *client*)))

(defun pop-input-handler ()
  (pop (input-handlers *client*)))

(defun change-top-handler (new-handler)
  (pop-input-handler)
  (push-input-handler new-handler))

(defmethod client-on-command ((client client) socket input)
;  (with-output-to-string (stream *out*)
;    (format stream "client-on-command: ~a ~a ~s~%" client socket input))
  (with-variables-from (globvars client)
      (*standard-output* *player-zone* *player-room* *player* *client*)
    (pomo:with-connection (append *db-connection-spec* '(:pooled-p t))
      (funcall (first (input-handlers client)) client input))))

(defmethod client-disconnect ((client client))
  (with-variables-from (globvars client)
      (*player-zone* *player-room* *player*)
    (when (eql (player-state client) :game)
      (deletef (players *player-room*) *player*)
      (set-user-offline *player*))))

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
     (when (not (and (= byte 255) (= byte prev-byte))) ;; hack for cp1251 YA character
       (vector-push-extend byte buffer))
     (setf prev-byte byte)))

(defun reset-buffer (client)
  (setf (fill-pointer (client-buffer client)) 0))

(defmethod client-read ((client client) socket)
;  (format t "client-read ~s ~s~%" client socket)
  (with-slots (buffer encoding) client
    (when (collect-input socket buffer)
      (prog1
          ;; FIXME: разобраться с ошибками разбора кодировки
	  (sb-ext:octets-to-string buffer :external-format encoding)
	(reset-buffer client)))))
