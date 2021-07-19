(in-package :cl-user)
(defpackage :lispmud/shop
  (:use :cl))
(in-package :lispmud/shop)

(defclass shop-room (service-room)
  ((price-list :initarg :price-list :initform nil
               :accessor price-list)))

(defclass seller (service-mob)
  ((buy-phrase :initarg :buy-phrase :initform ""
               :reader buy-phrase)
   (buy-see-others :initarg :buy-see-others :initform ""
                   :reader buy-see-others)
   (sell-phrase :initarg :sell-phrase :initform ""
                :reader sell-phrase)
   (sell-see-others :initarg :sell-see-others :initform ""
                    :reader sell-see-others)))
