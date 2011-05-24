(in-package :lispmud)

(defclass shop ()
  ((price-list :accessor price-list :initarg :price-list)
   (service-mob :accessor service-mob :initarg :service-mob)))