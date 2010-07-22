;;; main.lisp

(in-package :lispmud)

(defvar +localhost+ #(0 0 0 0))
(defvar +port+ 8000)
(defvar *world-filename* "world.lmud")
(defvar *zone-list*)
(defvar *player-output*)

(defun initialize-game ()
  (init-commands)
  (setf *zone-list* (load-world *world-filename*))
  (pvalue *zone-list*))

(defun main (&optional (port 3000))
  (initialize-game)
  (temp-start-work (first *zone-list*))
  (run-lispmud port))
