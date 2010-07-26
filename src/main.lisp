;;; main.lisp

(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)
(defvar *world-filename* "world.lmud")
(defvar *zone-list*)
(defvar *player-output*)
(defvar *savedir* "savedir/")

(defun initialize-game ()
  (init-commands)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun load-game-data ()
  (load-user-db (concatenate 'string *savedir* "users.db")))

(defun save-game-data ()
  (dump-user-db (concatenate 'string *savedir* "users.db")))

(defun main (&optional (port 3000))
  (define-case "мочалка" "молчалки" "мочалке" "мочалку" "мочалкой" "мочалке")
  (initialize-game)
  (load-game-data)
  (start-event-loop)
  (temp-start-work (first *zone-list*)) ;; Временная функция -- добаляет собаку на карту.
  (unwind-protect
      (run-lispmud port)
    (save-game-data)
    (stop-event-loop)))
