;;; threadvars.lisp

;; This file contains class for objects storing player variables.
;; Each player thread should have one special variable of this class. 

(defclass player-vars ()
  ((user-name :accessor user-name)
   (character-name :accessor character-name)
   (cur-room :accessor cur-room)
   (cur-zone :accessor cur-zone)))


