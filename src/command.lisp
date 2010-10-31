;;; command.lisp
;;;
;;; Command player console interface. All commands in Russian.
;;; Fasilities for definition of commands are situated in core-command.lisp.
;;;

(in-package :lispmud)

;; FIXME: move to other file.
(defun player-exited (room player)
  (iter (for p in (players room))
	(format (output p) "~&~a вышел из комнаты~%" (name player))))

(defun player-took (room player thing)
  (iter (for p in (remove player (players room)))
	(format (output p) "~&~a поднял с пола ~a и положил к себе в инвентарь.~%"
		(name player) (word-vp thing))))

(defun mudname-equal (short-name long-name)
  (iter (with short-name-length = (length short-name))
	(with long-name-length = (length long-name))
	(for start1 initially 0 then (1+ end1))
	(for start2 initially 0 then (1+ end2))
	(for end1 = (or (position #\.    short-name :start start1) short-name-length))
	(for end2 = (or (position #\Space long-name :start start2) long-name-length))
	(if (< (or (mismatch short-name long-name :start1 start1 :end1 end1 :start2 start2 :end2 end2) end1)
	       end1)
	    (return-from mudname-equal nil))
	(while (< end1 short-name-length)))
  t)

(defun command-store (&rest subcommand-and-names)
  (if subcommand-and-names
      (let ((subcomm (first subcommand-and-names))
	    (names (rest subcommand-and-names)))
	(word-dispatch subcomm
	  ("положить"
	   (iter (for name in names)
		 (for found-item = (find name (inventory *player*) :key #'name :test #'string-equal))
		 (print found-item)
		 (deletef (inventory *player*) found-item)
		 (put-to-store (name *player*) found-item)))
	  ("забрать"
	   (iter (for name in names)
		 (for found-item = (take-from-store (name *player*) name))
		 (push found-item (inventory *player*))))
	  (t (format t "~&Вы можете ПОЛОЖИТЬ вещь на слад или ЗАБРАТЬ её.~%"))))
      (format t "На сладе у вас лежат вещи:~%~{   ~A~%~}" (mapcar #'name (items-in-store (name *player*))))))


;(defun command-attack (defender)
;  (iter (for candidate in (append (mobs *player-room*) (players *player-room*)))
;	(when (mudname-equal defender candidate)
	  
	


(defun command-go-to-direction (direction)
  "Функции перехода по какому-нибудь направлению делаются каррированием этот функции."
  (assert (member direction *exits*))
  (let ((exit (exit *player-room* direction)))
    (if (exit *player-room* direction)
	(if (can-pass exit)
	    (progn
	      (deletef (players *player-room*) *player*)
	      (process-room-triggers *player-room* :player-left-room *player* *player-room* direction)
	      (player-exited *player-room* *player*)
	      (setf *player-room* (dest-room exit))
	      (push *player* (players *player-room*)))
	    (format t "К сожалению, проход в эту сторону для тебя закрыт.~%"))
	(format t "Вы не видите никакого прохода в этом направилении~%"))))

(defun command-leave ()
  (format t "До свидания, возвращайся быстрей!~%")
  (signal 'disconnect-client))

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

(defun command-map ()
  "Выводит на экран псевдографическую карту текущей зоны."
  (format t "~%")
  (iter (with map = (map-array *player-zone*))
	(for y from 0 below (array-dimension map 0))
	(format t " ")
	(with string = (make-string-output-stream))
	(format string " ")
	(iter (for x from 0 below (array-dimension map 1))
	      (if (aref map y x)
		  (progn 
		    (if (eql *player-room* (aref map y x))
			(progn
			  (color *cc-red*)
			  (format t "@")
			  (color *cc-reset*))
			(format t "#"))
		    (if (east-exit (aref map y x))
			(format t "-")
			(format t " "))
		    (if (south-exit (aref map y x))
			(format string "| ")
			(format string "  ")))
		  (progn
		    (format t "  ")
		    (format string "  "))))
	(format t "~%")
	(format string " ~%")
	(format t "~a" (get-output-stream-string string))))

(defun command-say (&rest words)
  "ГОВОРИТЬ: ваши слова услышат все в комнате"
  (iter (for p in (players *player-room*))
	(format (output p) "~a сказал: \"~{~a~^ ~}\"~%~%" (name *player*) words))
#+nil  (process-room-triggers *player-room* :player-said *player* *player-room* words))

(defun command-chat (&rest words)
  "БОЛТАТЬ: ваши слова услышат все игроки в игре"
  (iter (for p in *online-users*)
	(color *cc-green* (output p))
	(format (output p) "~a: \"~{~a~^ ~}\"~%~%" (name *player*) words)
	(color *cc-reset* (output p))))

;; FIXME: блокировка!
(defun command-take (&rest item-names)
  "Команда для подбирания вещей из текущей комнаты."
  (if item-names
      (iter (for item-name in item-names)
	    (with taken-items)
	    (iter (for item-on-floor in (items-on-floor *player-room*))
		  (when (mudname-equal item-name (name item-on-floor))
		    (push item-on-floor taken-items)
		    (take-item *player-room* *player* item-on-floor)))
	    (finally ;; Удалить поднятые шмотки из списка лежащих в комнате.
	     (setf (items-on-floor *player-room*) (nset-difference (items-on-floor *player-room*) taken-items))))
      (write-line "Что вы хотите взять-то?")))

(defun command-inventory ()
  "Комманда для просмотра вещей в инвентаре."
  (let ((inventory (inventory *player*)))
    (if inventory
	(progn
	  (color *cc-cyan*)
	  (write-line "Ваш инвентарь:")
	  (iter (for item in inventory)
		(format t "  ~a~%" (word-ip item)))
	  (color *cc-reset*))
	(write-line "У вас ничего нет. :("))))

(defun command-list-commands ()
  "Комманда для вывода списка других комманд"
  (format t
"Команды:
  север  -- идти на север
  юг     -- идти на юг
  запад  -- идти на запад
  восток -- идти на восток
  выходы -- перечислить выходы из комнаты
  оглядется -- напечатать подробное описание комнаты
  карта -- напечатать карту зоны
Работа с вещами:
  взять ВЕЩЬ -- взять ВЕЩЬ с пола
  инвентарь -- напечатать ваши вещи
  склад -- управляет складом
Общение
  ГОВОРИТЬ: ваши слова услышат все в комнате
  БОЛТАТЬ: ваши слова услышат все игроки в игре

  конец -- выйти из игры~%~%"))

(defun init-commands ()
  (init-command-table
   `(("эхо"  ,#'(lambda (&rest args)
		  (color *cc-red*)
		  (format t "~{~a ~}~%" args)
		  (color *cc-reset*)))
     ("ю" ,#'(lambda () (command-go-to-direction :south)))
     ("с" ,#'(lambda () (command-go-to-direction :north)))
     ("з" ,#'(lambda () (command-go-to-direction :west)))
     ("в" ,#'(lambda () (command-go-to-direction :east)))
     ("выходы" ,'command-exits)
     ("оглядеться" ,'command-look)
     ("карта" ,'command-map)
     ("взять" ,'command-take)
     ("инвентарь" ,'command-inventory)
     ("склад" ,'command-store)
     ("говорить" ,'command-say)
     ("болтать" ,'command-chat)
     ("команды" ,'command-list-commands)
     ("конец"  ,'command-leave))))
