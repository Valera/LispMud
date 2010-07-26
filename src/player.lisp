;;; player.lisp

(in-package :lispmud)

(defvar *player*)

(defclass player ()
  ((output :accessor output :initarg :output)
   (inventory :accessor inventory :initform nil)
   (name :accessor name :initarg :name)))
