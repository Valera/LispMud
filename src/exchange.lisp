;;; exchange.lisp

(in-package :cl-user)
(defpackage :lispmud/exchange
  (:use :cl)
  (:import-from :bt #:make-lock)
  (:import-from :lispmud/core-utils #:with-hash-table-value #:string-join))
(in-package :lispmud/exchange)

(defvar *max-exchange-items* 50
  "Максимально число лотов на бирже от одного игрока")

(defvar *exchange* (make-hash-table :test 'equal))

(defvar *exchange-lock* (make-lock "Exchange"))

(defun add-stock (person item price)
  (with-hash-table-value 
      (item-list person *exchange*)
    (if (< (length item-list) *max-exchange-items*)
	(format t (string-join
		   "Извините, но вы уже выставили на биржу максимальное число лотов." #\Nl
		   "Чтобы выставить эту вещь, снимите что-нибудь с продажи." #\Nl))
	(progn
	  (push (list item price) *exchange*)
	  (format t "~a выставлена на биржу~%" item)))))

(defun reset-price (person item new-price)
  (with-hash-table-value 
       (item-list person *exchange*)
    (let ((item-price (find item item-list :key #'first)))
      (if item-price
	  (progn
	    (setf (second item-price) new-price)
	    (format t "Цена изменена~%"))
	  (format t "Лот не найден~%")))))

(defun buy-stock ())

(defun stock-info ())

(defun stock-fullinfo ())
