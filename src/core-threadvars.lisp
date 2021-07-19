;;; threadvars.lisp

(in-package :cl-user)
(defpackage :lispmud/core-threadvars
  (:use :cl)
  (:export #:*player* #:*player-room* #:*player-zone* #:*zone-list* #:*client*))
(in-package :lispmud/core-threadvars)

;; Данный файл содержи объявления динамических переменных, описывающих состояние
;; конкретного игрока. Для каждой игровой нити инициализируются в соотвествии с
;; информацией об игроке. 

;; Вся информация об игроке содержится в этих переменных. Класс с фукнциями доступа
;; делать не очень удобно, так как их придётся слишком часто использовать.

(defvar *player*      nil "Объект игрока")
(defvar *player-room* nil "Текущая комната игрока")
(defvar *player-zone* nil "Текущая зона игрока")
(defvar *client* nil "Объект, подключающий игрока к игре")

(defvar *zone-list*)
