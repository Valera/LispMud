;;; items.lisp

(in-package :lispmud)

(defclass item ()
  ((name :accessor name :initarg :name
         :initform (error "Can not create ITEM without a name"))
   (description :accessor description)
   (price :accessor price)))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "\"~A\"" (name obj))))

(defgeneric take-item (room player item))
(defmethod take-item :after (room player (item item))
  (iter (for p in (remove player (players room)))
	(format (output p) "~&~a поднял с пола ~a и положил к себе в инвентарь.~%"
		(name player) (word-vp item))))

(defmethod take-item (room player (item item))
  (push item (inventory player))
  (format t "Вы взяли с пола ~a.~%" (word-vp item)))

(defgeneric copy-obj (x))
(defmethod copy-obj ((item item))
  (make-instance (class-of item) :name (name item)
                 :description (description item)
                 :price (price item)))

(defclass coin-heap (item)
  ((coins :accessor coins :initarg :coins))
  (:default-initargs :name "кучка монет"))

(defmethod take-item (room player (coin-heap coin-heap))
  (format t "Вы взяли с пола ~a монет.~%" (coins coin-heap))
  (incf (money player) (coins coin-heap)))

(defclass letter (item)
  ((text :accessor text :initarg :text))
  (:default-initargs :name "письмо"))

(defvar *items*
  '((food :name "Чёрствый хлеб" :nutritiousness 50)
    (food :name "Батон" :nutritiousness 100)
    (food :name "Буханка" :nutritiousness 90)
    (food :name "Булка с маком" :nutritiousness 150)))


;(defclass food (item)
;  ((

;(defclass wearable-item ()
;   (min-level :accessor min-level))

;(defgeneric can-wear-p (player item))

;(defmethod can-wear-p ((player player) (item wearable-item))
;  (>= (level player) (min-level item)))

;(defclass weapon-item (item)
;  (damage :accessor damage))

;(defclass armour-item (wearable-item)
;  (armour-class :accessor armour-class)
