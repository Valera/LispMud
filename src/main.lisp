;;; main.lisp

(in-package :lispmud)

(defvar *host*)
(defvar *port*)
(defvar *world-filename*)
(defvar *savedir*)
(defvar *enter-password*)

(defun load-config (file-name &key port-arg)
  (with-input-from-file (stream file-name)
    (destructuring-bind (&key host port world-file save-directory enter-password)
	(read stream)
;      (break "~a ~a ~a ~a ~a" host port world-file save-directory)
      (unless (and host port world-file save-directory)
	(error "Конфигурационный файл ~S не содержит все необходимые настройки" file-name))
      (setf *host* host
	    *port* (or port-arg port)
	    *world-filename* world-file
	    *savedir* save-directory
	    *enter-password* enter-password))))

(defun initialize-game ()
  (reset-online-users)
  (init-commands)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun load-game-data ()
  (load "src/cases.lisp")
  (flet ((full-path (fname) (concatenate 'string *savedir* fname)))
    (load-user-db (full-path "users.db"))
    (load-store   (full-path "store.db"))))

(defun save-game-data ()
  (flet ((full-path (fname) (concatenate 'string *savedir* fname)))
    (dump-user-db (full-path "users.db"))
    (dump-store   (full-path "store.db"))))

(defun main (&key (config-file "sample-config.lisp") port)
  (load-config config-file :port-arg port)
  (initialize-game)
  (load-game-data)
  (start-event-loop)
  (temp-start-work (first *zone-list*)) ;; Временная функция -- добаляет собаку на карту.
  (unwind-protect
      (run-lispmud *port* :host *host*)
    (save-game-data)
    (stop-event-loop)))
