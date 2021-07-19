;;; player.lisp

(in-package :cl-user)
(defpackage :lispmud/player
  (:use :cl)
  (:import-from :lispmud/core-utils #:name))
(in-package :lispmud/player)

(defvar *player*)

(defclass player ()
  ((output :accessor output :initarg :output)
   (inventory :accessor inventory :initform nil)
   (money :accessor money :initform 0)
   (name :accessor name :initarg :name)))
