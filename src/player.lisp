;;; player.lisp

(in-package :lispmud)

(defvar *player*)

(defclass player ()
  ((inventory :accessor inventory :initform nil)
   (name :accessor name :initarg :name)))

