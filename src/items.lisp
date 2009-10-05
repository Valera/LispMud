;;; items.lisp

(in-package :lispmud)

(defclass item ()
  ((name :accessor name)
   (description :accessor description)
   (price :accessor price)))

(defclass wearable-item ()
   (min-level :accessor min-level))

(defgeneric can-wear-p (player item))

(defmethod can-wear-p ((player player) (item wearable-item))
  (>= (level player) (min-level item)))

(defclass weapon-item (item)
  (damage :accessor damage))

(defclass armour-item (wearable-item)
  (armour-class :accessor armour-class)
