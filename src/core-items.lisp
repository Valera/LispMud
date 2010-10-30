;;; items.lisp

(in-package :lispmud)

(defclass item ()
  ((name :accessor name :initarg :name)
   (description :accessor description)
   (price :accessor price)))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "\"~A\"" (name obj))))

;(defclass wearable-item ()
;   (min-level :accessor min-level))

;(defgeneric can-wear-p (player item))

;(defmethod can-wear-p ((player player) (item wearable-item))
;  (>= (level player) (min-level item)))

;(defclass weapon-item (item)
;  (damage :accessor damage))

;(defclass armour-item (wearable-item)
;  (armour-class :accessor armour-class)