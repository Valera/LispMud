;;; command.lisp

(in-package :lispmud)

(defun command-go-to-direction (direction)
  "Функции перехода по какому-нибудь направлению делаются каррированием этот функции."
  (assert (member direction *exits*))
  (let ((exit (exit *player-room* direction)))
    (if (exit *player-room* direction)
	(if (can-pass exit)
	    (setf *player-room* (dest-room exit))
	    (format t "К сожалению, проход в эту сторону для тебя закрыт.~%"))
	(format t "Вы не видите никакого прохода в этом направилении~%"))))

(defun command-leave ()
  (format t "До свидания, возвращайся быстрей!~%")
  (setf *player-exit-flag* t))

(defun command-look ()
  "комманда, печатающая которкие описания выходов"
  (dolist (direction *exits*)
    (if (exit *player-room* direction)
	(format t "  ~a~t: ~a~%"
		(case direction (:north "cевер") (:east "восток") (:south "юг") (:west "запад"))
		(short-description (dest-room (exit *player-room* direction)))))))

(defun command-exits ()
  "комманда, печатающая список выходов из комнаты"
  (let 
      ((exits (remove nil (list (if (north-exit *player-room*) "север" nil)
				(if (south-exit *player-room*) "юг" nil)
				(if (west-exit *player-room*) "запад" nil)
				(if (east-exit *player-room*) "восток" nil)))))
    (format t "Вы видите выходы на ~{~a~^, ~}.~%" exits)))

(defun init-commands ()
  (init-command-table
   `(("эхо"  ,#'(lambda (&rest args) (format t "~{|~a| ~}~%" args)))
     ("ю" ,#'(lambda () (command-go-to-direction :south)))
     ("с" ,#'(lambda () (command-go-to-direction :north)))
     ("з" ,#'(lambda () (command-go-to-direction :west)))
     ("в" ,#'(lambda () (command-go-to-direction :east)))
     ("выходы" ,'command-exits)
     ("оглядеться" ,'command-look)
     ("конец"  ,'command-leave))))