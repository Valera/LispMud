;;; main.lisp

(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)
(defvar *world-filename* "world.lmud")
(defvar *savedir* "savedir/")

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

(defun main (&optional (port 3000))

  (initialize-game)
  (load-game-data)
  (start-event-loop)
  (temp-start-work (first *zone-list*)) ;; Временная функция -- добаляет собаку на карту.
  (unwind-protect
      (run-lispmud port)
    (save-game-data)
    (stop-event-loop)))
