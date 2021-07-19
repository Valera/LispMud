;;; rucase.lisp
;;;
;;; Case system for Russian language, no interest for other lagnuage speakers.
;;; Система для работы с падежами в русском языке.
;;;

(in-package :cl-user)
(defpackage :lispmud/rucase
  (:use :cl)
  (:import-from :alexandria #:last-elt)
  (:import-from :iter
                #:iter #:for #:in #:while)
  (:import-from :lispmud/core-utils
                #:name #:pvalue)
  (:export #:word-ip #:word-rp #:word-dp #:word-vp #:word-tp #:word-pp))
(in-package :lispmud/rucase)

(defvar *case-hash* (make-hash-table :test 'equal)
  "Хеш-таблица, ставящаа в соотвествие слову в именительном падеже вектор из всех шести падежей этого слова, включая сам именительный")

(defun all-russian-letters-p (namestring)
  "Возвращает T, если все буквы в строке являются строчными и заглавными буквами руссокго языка."
  (every #'(lambda (c) (char-not-greaterp #\А c #\Я)) namestring))

(defun define-case (ip rp dp vp tp pp &key accept-non-russian-letters)
  "Регистрирует слово и его падежи. По умолнчанию сигнализирует ошибку,
если все строки не состоят целиком из русских букв. Эту проверку
можно отключить, передав :accept-non-russian-letters T."
  (unless accept-non-russian-letters
    (assert (every #'all-russian-letters-p (list ip rp dp vp tp pp))
	    (ip rp dp vp tp pp) "Одна из строк с падежной формой содержит не только русские буквы."))
  (setf (gethash ip *case-hash*)
	(vector ip rp dp vp tp pp)))

(defun print-case-forms (word)
  (format t
	  "есть    ~a~%нет     ~a~%дать    ~a~%винить  ~a~%доволен ~a~%думаю о ~a~%"
	  (word-ip word) (word-rp word) (word-dp word) 
	  (word-vp word) (word-tp word) (word-pp word)))

;; FIXME молоток

(defun define-case-auto (ip case-number &key accept-non-russian-letters)
  "Автоматическое определение слова с падеже (1, 2, 3) case-number"
  (unless accept-non-russian-letters
    (assert (all-russian-letters-p ip) (ip) "Слово ~a содержит не только русские буквы." ip))
  (ecase case-number
    (2 (let ((base (if (char= #\о (last-elt ip)) (subseq ip 0 (1- (length ip))) ip)))
	 (print base)
	 (setf (gethash ip *case-hash*)
	       (flet ((ending (letters) (concatenate 'string base letters)))
		 (print
		  (vector ip (ending "а") (ending "у") ip (ending "ом") (ending "е")))))))
    (1 (let ((last-char (last-elt ip)))
	 (if (member last-char '(#\CYRILLIC_SMALL_LETTER_IE #\CYRILLIC_CAPITAL_LETTER_IE
				 #\CYRILLIC_SMALL_LETTER_A #\CYRILLIC_CAPITAL_LETTER_A))
	     (let ((base (subseq ip 0 (1- (length ip)))))
	       (setf (gethash ip *case-hash*)
		     (flet ((ending (letters) (concatenate 'string base letters)))
		       (print
			(vector ip 
				(if (member (last-elt base) '(#\CYRILLIC_SMALL_LETTER_KA) :test #'char-equal)
				    (ending "и")
				    (ending "ы"))
				(ending "е") (ending "у") (ending "ой") (ending "е"))))))
	     (error "Падёж слова ~S задан как ~a, но в этом падеже слово оканчивается на а или е." ip case-number))))))

(defun word-in-case (word-identifier case-index)
  "Возвращает падеж слова word-identifier в падеже, соответствующем case-index.
0 -- именительный, 1 -- родительный, 2 -- дательный, 3 -- винительный,
4 -- творительный, 5 -- предложный. Не рекомендуется использовать, используйте
семейство функций word-*."
  (let* ((word (if (stringp word-identifier)
		   word-identifier
		   (name word-identifier)))
	 (word-vector (gethash word *case-hash*)))
    (if word-vector
	(aref word-vector case-index)
	(format nil "|Падёж слова ~a не задан|" word-identifier))))

(defun russian-vowel-p (char)
  (member char '(#\CYRILLIC_SMALL_LETTER_A
		 #\CYRILLIC_SMALL_LETTER_IE 
		 #\CYRILLIC_SMALL_LETTER_IO 
		 #\CYRILLIC_SMALL_LETTER_I 
		 #\CYRILLIC_SMALL_LETTER_O 
		 #\CYRILLIC_SMALL_LETTER_U 
		 #\CYRILLIC_SMALL_LETTER_YERU 
		 #\CYRILLIC_SMALL_LETTER_E 
		 #\CYRILLIC_SMALL_LETTER_YU 
		 #\CYRILLIC_SMALL_LETTER_YA
		 #\CYRILLIC_CAPITAL_LETTER_A
		 #\CYRILLIC_CAPITAL_LETTER_IE 
		 #\CYRILLIC_CAPITAL_LETTER_IO 
		 #\CYRILLIC_CAPITAL_LETTER_I 
		 #\CYRILLIC_CAPITAL_LETTER_O 
		 #\CYRILLIC_CAPITAL_LETTER_U 
		 #\CYRILLIC_CAPITAL_LETTER_YERU 
		 #\CYRILLIC_CAPITAL_LETTER_E 
		 #\CYRILLIC_CAPITAL_LETTER_YU 
		 #\CYRILLIC_CAPITAL_LETTER_YA)))

(defun russian-consonantal-p (char)
  (member char '(#\CYRILLIC_CAPITAL_LETTER_BE 
		 #\CYRILLIC_CAPITAL_LETTER_VE 
		 #\CYRILLIC_CAPITAL_LETTER_GHE 
		 #\CYRILLIC_CAPITAL_LETTER_DE 
		 #\CYRILLIC_CAPITAL_LETTER_ZHE 
		 #\CYRILLIC_CAPITAL_LETTER_ZE 
		 #\CYRILLIC_CAPITAL_LETTER_KA 
		 #\CYRILLIC_CAPITAL_LETTER_EL 
		 #\CYRILLIC_CAPITAL_LETTER_EM 
		 #\CYRILLIC_CAPITAL_LETTER_EN 
		 #\CYRILLIC_CAPITAL_LETTER_PE 
		 #\CYRILLIC_CAPITAL_LETTER_ER 
		 #\CYRILLIC_CAPITAL_LETTER_ES 
		 #\CYRILLIC_CAPITAL_LETTER_TE 
		 #\CYRILLIC_CAPITAL_LETTER_EF 
		 #\CYRILLIC_CAPITAL_LETTER_HA 
		 #\CYRILLIC_CAPITAL_LETTER_TSE 
		 #\CYRILLIC_CAPITAL_LETTER_CHE 
		 #\CYRILLIC_CAPITAL_LETTER_SHA 
		 #\CYRILLIC_CAPITAL_LETTER_SHCHA
		 #\CYRILLIC_SMALL_LETTER_BE 
		 #\CYRILLIC_SMALL_LETTER_VE 
		 #\CYRILLIC_SMALL_LETTER_GHE 
		 #\CYRILLIC_SMALL_LETTER_DE 
		 #\CYRILLIC_SMALL_LETTER_ZHE 
		 #\CYRILLIC_SMALL_LETTER_ZE 
		 #\CYRILLIC_SMALL_LETTER_KA 
		 #\CYRILLIC_SMALL_LETTER_EL 
		 #\CYRILLIC_SMALL_LETTER_EM 
		 #\CYRILLIC_SMALL_LETTER_EN 
		 #\CYRILLIC_SMALL_LETTER_PE 
		 #\CYRILLIC_SMALL_LETTER_ER 
		 #\CYRILLIC_SMALL_LETTER_ES 
		 #\CYRILLIC_SMALL_LETTER_TE 
		 #\CYRILLIC_SMALL_LETTER_EF 
		 #\CYRILLIC_SMALL_LETTER_HA 
		 #\CYRILLIC_SMALL_LETTER_TSE 
		 #\CYRILLIC_SMALL_LETTER_CHE 
		 #\CYRILLIC_SMALL_LETTER_SHA 
		 #\CYRILLIC_SMALL_LETTER_SHCHA)))

;; Вернуть слово в соответстующем падеже.ы
(defun word-ip (word-identifier)     (word-in-case word-identifier 0))
(defun word-rp (word-identifier)     (word-in-case word-identifier 1))
(defun word-dp (word-identifier)     (word-in-case word-identifier 2))
(defun word-vp (word-identifier)     (word-in-case word-identifier 3))
(defun word-tp (word-identifier)     (word-in-case word-identifier 4))
(defun word-pp (word-identifier)     (word-in-case word-identifier 5))

;; INCOMPLETE.
(defun number-in-russian (number &key (sex :male))
  (assert (member sex '(:male :female :neuter)) (sex))
  (let ((0-to-19 #("ноль" "одна" "два" "три" "четыре" "пять"
		   "шесть" "семь" "восемь" "девять" "десять"
		   "одиннадцать" "двенадцать" "тринадцать" "четырнадцать" "пятнадцать"
		   "шестнадцать" "семнадцать" "восемнадцать" "девятнадцать"))
	(tens #("неверный индекс" "десять" "двадцать" "тридцать" "сорок" "пятьдесят"
		"шестьдесят" "семьдесят" "восемьдесят" "девяносто"))
	(hundreds #("неверный индекс" "сто" "двести" "триста" "четыреста" "пятьсот"
		    "шестьсот" "семьсот" "восемьсот" "девятьсот"))
	(thousand-powers #("" "тысяча" "миллион" "миллиард" "биллион"
			   "триллион" "квадриллион" "квинтиллион"
			   "секстиллион" "септиллион" "октиллион" "нониллион" "дециллион")))
    (labels ((thousand-power-name (number)
	       (aref thousand-powers (floor (log number 1000))))
	     (ten-name (number)
	       (aref tens (rem (rem number 10) 10)))
	     (hundred-name (number)
	       (aref hundreds (floor number 100)))
	     (small-number-to-stream (number stream)
	       "Маленькое (< 1000) положительно число"
	       (if (>= number 100)
		   (format stream "~a " (hundred-name number)))
	       (if (< number 20)
		   (format stream "~A " (aref 0-to-19 number))
		   (if (plusp (rem number 10))
		       (format stream "~A ~A" (ten-name number) (aref 0-to-19 (rem number 10)))
		       (format stream "~A" (ten-name number))))))
      (with-output-to-string (string)
	(when (minusp number)
	  (write "минус " :stream string))
	(let ((number (abs number)))
	  (iter (for num initially number then (rem num power-of-1000))
		(for power-of-1000 = (expt 1000 (floor (log num 1000))))
		(while (> num 1000))
		(pvalue num)
		(pvalue  power-of-1000)
		(for mantissa = (floor num power-of-1000))
		(pvalue mantissa)
		(pvalue (hundred-name mantissa))
		(small-number-to-stream mantissa string)
		(format string " ~a " (thousand-power-name num))
		(format string "|"))
	  (small-number-to-stream (rem number 1000) string))))))
