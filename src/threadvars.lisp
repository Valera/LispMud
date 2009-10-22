;;; threadvars.lisp

;; This file contains class for container of player variables.
;; Each player thread should have one special variable of this class. 

(in-package :lispmud)

(defvar *thread-vars* nil)

(defclass thread-vars ()
  ((user-name :accessor user-name)
   (character-name :accessor character-name)
   (cur-room :accessor cur-room)
   (cur-zone :accessor cur-zone)
   (end-p :accessor end-p :initform nil)))


