;;; items.lisp

(in-package :cl-user)
;; TODO: candidate to moving everything to items
(defpackage :lispmud/core-items
  (:use :cl)
  (:import-from :alexandria #:deletef)
  (:import-from :lispmud/core-utils #:name)
  (:import-from :lispmud/rucase #:word-vp)
  (:import-from :lispmud/core-room #:message-to-visitors-except #:*default-items-on-floor*)
  (:import-from :lispmud/player #:money #:inventory #:output))
(in-package :lispmud/core-items)

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
  (message-to-visitors-except
   player room (format (output player) "~&~a поднял с пола ~a и положил к себе в инвентарь.~%"
                       (name player) (word-vp item))))

(defmethod take-item (room player (item item))
  (push item (inventory player))
  #+nil
  (with-items-collection items
    (mongo:insert-op items (son "_id"
                            "name" (name item)
                                "class" (class-of item)
                                "name" (name item)
                                "description" (description item)
                                "price" (price item))))
  (format t "Вы взяли с пола ~a.~%" (word-vp item)))

(defgeneric drop-item (room player item))
(defmethod drop-item (room player (item item))
  (deletef (inventory player) item)
  (format t "Вы бросили ~A на пол.~%" (word-vp item)))

(defmethod drop-item :after (room player (item item))
  (message-to-visitors-except
   player room (format nil "~&~a поднял с пола ~a и положил к себе в инвентарь.~%"
                       (name player) (word-vp item))))

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

(defparameter *default-items-on-floor*
  (list
   (make-instance 'coin-heap :coins (1+ (random  9)))
   (make-instance 'item :name "мочалка")
   (make-instance 'item :name "кусок мыла")))

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
