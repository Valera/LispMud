;;; rucase.lisp
;;;
;;; Case system for Russian language, no interest for other lagnuage speakers.
;;; Система для работы с падежами в русском языке.
;;;

(in-package :lispmud)

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

(defun word-ip (word-identifier)     (word-in-case word-identifier 0))
(defun word-rp (word-identifier)     (word-in-case word-identifier 1))
(defun word-dp (word-identifier)     (word-in-case word-identifier 2))
(defun word-vp (word-identifier)     (word-in-case word-identifier 3))
(defun word-tp (word-identifier)     (word-in-case word-identifier 4))
(defun word-pp (word-identifier)     (word-in-case word-identifier 5))
