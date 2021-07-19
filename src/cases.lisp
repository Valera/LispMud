(in-package :cl-user)
(defpackage :lispmud/cases
  (:use :cl)
  (:import-from :lispmud/rucase #:define-case #:define-case-auto))
(in-package :lispmud/cases)

;; TODO: это не должно быть лисповым кодом, это данные!

(define-case "мочалка" "молчалки" "мочалке" "мочалку" "мочалкой" "мочалке")
(define-case "Панда" "Панды" "Панде" "Панду" "Пандой" "Панде")
(define-case "кучка монет" "кучки монет" "кучке монет" "кучку монет" "кучкой монет" "кучке монет" :accept-non-russian-letters t)
(define-case-auto "север" 2)
(define-case-auto "юг" 2)
(define-case-auto "восток" 2)
(define-case-auto "запад" 2)
(define-case-auto "письмо" 2)

#+nil
(defun prepend-number (number word)
  (cond
    ((= 1 number)
     (format nil "~a" word-ip))
    ((<= 2 number 4)
     (format nil ""
))))
