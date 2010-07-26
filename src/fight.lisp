;;; fight.lisp
;;;
;;; Файл с описанием процесса боя между игроками и мобаби, игроками и игроками.
;;;

(defgeneric attack (atacker defender)
  (:documentation "Функция, выполняющая атаку atacker'а на defender'а."))

;(defmethod attack ((atacker player) (defender mob))
;  (decf (hp mob) (roll-damage player))
;  (if (minusp (hp mob))
;      (format t 
;)
