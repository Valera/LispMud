;;; main.lisp

(in-package :cl-user)
(defpackage :lispmud/main
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:use :lispmud/core-globalvars)
  (:import-from :alexandria #:with-input-from-file)
  (:import-from :lispmud/core-utils #:pvalue)
  (:import-from :lispmud/userdb #:reset-online-users)
  (:import-from :lispmud/command #:init-commands)
  (:import-from :lispmud/world #:load-world)
  (:import-from :lispmud/event-timer #:start-event-loop #:stop-event-loop)
  (:import-from :lispmud/core-zone #:temp-start-work)
  (:import-from :lispmud/core-server #:run-lispmud))
(in-package :lispmud/main)

(defvar *host*)
(defvar *port*)
(defvar *world-filename*)
(defvar *savedir*)

(defun load-config (file-name &key port-arg)
  (with-input-from-file (stream file-name)
    (destructuring-bind
	  (&key host port world-file save-directory alpha-version-password
		db-name db-user db-password db-host)
	(read stream)
;      (break "~a ~a ~a ~a ~a" host port world-file save-directory)
      (unless (and host port world-file save-directory)
	(error "Конфигурационный файл ~S не содержит все необходимые настройки" file-name))
      (setf *host* host
	    *port* (or port-arg port)
	    *world-filename* world-file
	    *savedir* save-directory
	    *alpha-version-password* alpha-version-password
	    *db-connection-spec* (list db-name db-user db-password db-host)))))

(defun initialize-game ()
  (reset-online-users)
  (init-commands)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun load-game-data ()
  (load "src/cases.lisp"))

(defun save-game-data ())

(defun main (&key (config-file "sample-config.lisp") port)
  (load-config config-file :port-arg port)
  (pomo:with-connection (append *db-connection-spec* '(:pooled-p t))
    (initialize-game)
    (load-game-data)
    (start-event-loop)
    (temp-start-work (first *zone-list*)) ;; Временная функция -- добаляет собаку на карту.
    (pvalue bt:*default-special-bindings*)
    (unwind-protect
	 (run-lispmud *port* :host *host*)
      (save-game-data)
      (stop-event-loop))))
