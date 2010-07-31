;;; fight.lisp
;;;
;;; Файл с описанием процесса боя между игроками и мобаби, игроками и игроками.
;;;

(in-package :lispmud)

(defgeneric attack (attacker defender)
  (:documentation "Функция, выполняющая атаку attacker'а на defender'а."))

(defmethod attack ((attacker player) (defender mob))
  (decf (hp defender) 1)
  (if (<= (hp defender) 0)
      (format t "~A умер." (word-ip defender))))
