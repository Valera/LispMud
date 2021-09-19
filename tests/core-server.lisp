(in-package :cl-user)
(defpackage :lispmud/tests/core-server
  (:use :cl)
  (:use :usocket)
  (:use :lispmud/core-threadvars)
  (:import-from :lispmud/core-utils #:pvalue)
  (:import-from :lispmud/core-zone #:zone)
  (:import-from :lispmud/core-server
                #:server #:add-zone-worker #:provider-thread #:shutdown-workers #:stop-server))
(in-package :lispmud/tests/core-server)

(5am:def-suite core-server
  :in :lispmud)

(5am:in-suite core-server)

(defvar *test-zone-symbol* :test-zone)
(defparameter *test-port* 8510)

(5am:test server-smoke-test
  "Create server, add worker, check thread exists, shutdown workers, check thread was terminated"
  (let ((n-threads (length (bt:all-threads)))
        (serv (make-instance 'server)))
    (add-zone-worker serv *test-zone-symbol*)
    ;; (provider-thread serv master-socket)
    (5am:is (eql (1+ n-threads) (length (bt:all-threads))) "Worker thread must exist")
    (shutdown-workers serv)
    (sleep 0.01)
    (5am:is (eql n-threads (length (bt:all-threads))) "Worker thread must be terminated")))

(defun %read-all-available-bytes (stream)
  "Read all available to this moment bytes from stream."
  ;(let ((buffer (make-array 200 :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (let ((buffer (make-array 200 :fill-pointer 0 :element-type 'character)))
      (loop
        :while (listen stream)
        :doing (vector-push-extend (read-char stream) buffer))
      buffer))

(5am:test start-server-and-process-one-conection
  (let ((lispmud/main::*world-filename* (uiop:merge-pathnames* "content/world.lmud"
                                                 (asdf:system-source-directory :lispmud))))
    ;; TODO: better config names, maybe test section inside one config?
    (lispmud/main::load-config "test-config.lisp" :port-arg *test-port*) ;; FIXME we are not testing main.lisp here
    (lispmud/main::initialize-game) ;; FIXME: should not be here; we are not testing main.lisp here
    (let ((n-threads (length (bt:all-threads)))
          (serv (make-instance 'server))
          (master-socket (socket-listen #(127 0 0 1) *test-port* :element-type 'unsigned-byte :reuse-address t))
          (client-socket-no-disconnect nil)
          provider-thread)
      (unwind-protect
           (progn
             (pvalue master-socket)
             (add-zone-worker serv *test-zone-symbol*)
             (setf provider-thread (bt:make-thread #'(lambda () (provider-thread serv master-socket))))
             (5am:is (eql (+ 2 n-threads) (length (bt:all-threads))))
             (setf client-socket-no-disconnect (usocket:socket-connect #(127 0 0 1) *test-port*))
               (let ((client-socket (usocket:socket-connect #(127 0 0 1) *test-port*)))
                 (unwind-protect
                      (progn
                        (sleep 0.05)
                        (let ((welcome-string (%read-all-available-bytes (socket-stream client-socket))))
                          (format t "~&read bytes: ~s~%" welcome-string)
                          (5am:is (search "Добро пожаловать" welcome-string))))
                   (socket-close client-socket))))
             (stop-server serv master-socket provider-thread))
        (5am:is (eql n-threads (length (bt:all-threads))))
        ;; Check creating socket on the same port is not blocked.
        (socket-close (socket-listen #(127 0 0 1) *test-port* :element-type 'unsigned-byte :reuse-address t))
        (socket-close client-socket-no-disconnect))))

;; TODO: создать сервер, добавить зону, обработать одно событие таймера
