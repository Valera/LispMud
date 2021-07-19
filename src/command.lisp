;;; command.lisp
;;;
;;; Command player console interface. All commands in Russian.
;;; Fasilities for definition of commands are situated in core-command.lisp.
;;;

(in-package :cl-user)
(defpackage :lispmud/command
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:use :lispmud/rucase)
  (:import-from :iter #:iter #:for #:with #:while #:finally)
  (:import-from :alexandria #:deletef)
  (:import-from :lispmud/core-utils #:pvalue #:name)
  (:import-from :lispmud/core-command #:word-dispatch #:init-command-table)
  (:import-from :lispmud/core-server #:push-input-handler #:disconnect-client)
  (:import-from :lispmud/bank #:deposit #:withdraw #:balance #:transfer)
  (:import-from :lispmud/mob #:deliver-mail-for)
  (:import-from :lispmud/shop #:price-list)
  (:import-from :lispmud/core-items #:take-item #:text #:price #:copy-obj)
  (:import-from :lispmud/core-fsm #:generate-fsm #:next-state #:fsm #:input #:process-input1)
  (:import-from :lispmud/core-mail #:send-mail)
  (:import-from :lispmud/color-codes #:*cc-red* #:*cc-green*
                #:*cc-cyan* #:color #:*cc-reset*)
  (:import-from :lispmud/player #:output #:inventory #:money)
  (:import-from :lispmud/userdb #:online-user-names #:user-exists-p)
  (:import-from :lispmud/input-handlers #:*room-changed*)
  (:import-from :lispmud/store #:take-from-store #:put-to-store #:items-in-store)
  (:import-from :lispmud/core-zone #:map-array)
  (:import-from :lispmud/core-room #:description #:short-description
                #:dest-room #:east-exit #:west-exit #:north-exit #:south-exit
                #:editor-info #:place-type #:myroom #:reverse-direction
                #:dx-for-direction #:dy-for-direction #:*exits* #:direction
                #:exit #:exit-slot-for-direction #:mobs #:triggers #:players
                #:can-pass #:process-room-triggers #:items-on-floor))
(in-package :lispmud/command)

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

(defun find-in-inventory (item-name &optional (player *player*))
  (find item-name (inventory player) :key #'name :test #'mudname-equal))

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
	      (push *player* (players *player-room*))
	      (setf *room-changed* t))
	    (format t "К сожалению, проход в эту сторону для тебя закрыт.~%"))
	(format t "Вы не видите никакого прохода в этом направилении~%"))))

(defun command-leave ()
  (format t "До свидания, возвращайся быстрей!~%")
  (signal 'disconnect-client))

(defun command-look ()
  "комманда, печатающая короткие описания выходов"
  (dolist (direction *exits*)
    (if (exit *player-room* direction)
	(format t "  ~a~t: ~a~%"
		(case direction (:north "cевер") (:east "восток") (:south "юг") (:west "запад"))
		(short-description (dest-room (exit *player-room* direction)))))))

(defun command-exits (&rest ignored-args)
  "комманда, печатающая список выходов из комнаты"
  (declare (ignore ignored-args))
  (let 
      ((exits (remove nil (list (if (north-exit *player-room*) "север" nil)
				(if (south-exit *player-room*) "юг" nil)
				(if (west-exit *player-room*) "запад" nil)
				(if (east-exit *player-room*) "восток" nil)))))
    (format t "Вы видите выходы на ~{~a~^, ~}.~%" exits)))

(defun command-map (&rest ignored-args)
  "Выводит на экран псевдографическую карту текущей зоны."
  (declare (ignore ignored-args))
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
  (iter (for p in (online-user-names))
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

(defun command-inventory (&rest args)
  "Команда для просмотра вещей в инвентаре."
  (declare (ignore args))
  (let ((inventory (inventory *player*)))
    (if inventory
	(progn
	  (color *cc-cyan*)
	  (write-line "Ваш инвентарь:")
	  (iter (for item in inventory)
		(format t "  ~a~%" (word-ip item)))
	  (color *cc-reset*))
	(write-line "У вас ничего нет. :("))))

(defun command-deposit (&rest sum-string-arg)
  "Команда ВЛОЖИТЬ: вложить N монет в банк."
  (if (= 1 (length sum-string-arg))
      (let ((sum (parse-integer (first sum-string-arg))))
        (if (and (plusp sum) (<= sum (money *player*)))
            (progn
              (decf (money *player*) sum)
              (deposit (name *player*) sum)
              (process-room-triggers *player-room* :bank-deposit-trigger *player* sum))
            (format t "Извините, вложить в банк \"~a\" нельзя.~%"
                    (first sum-string-arg))))
      (wrong-command t "ВЛОЖИТЬ" "<ЧИСЛО>")))

(defun wrong-command (stream command-name &rest command-args)
  (format stream "Неправильный вызов комманды ~A. Чтобы воспользоваться коммандой,~%"
          command-name)
  (format stream "введите~%        ~A ~{~A ~}~%" command-name command-args))

(defun command-withdraw (&rest sum-string-arg)
  "Комманда СНЯТЬ: снять N монет со счёта в банке."
  (if (= 1 (length sum-string-arg))
      (let ((sum (parse-integer (first sum-string-arg))))
        (if (and (plusp sum) (<= sum (balance (name *player*))))
            (if (withdraw (name *player*) sum)
                (incf (money *player*) sum)
                (format t "Извините. На вашем счёте слишком мало денег."))
            (format t "Извините, но вы не можете снять со счёта \"~a\" монет.~%"
                    (first sum-string-arg))))
      (wrong-command t "СНЯТЬ" "<ЧИСЛО>")))

(defun command-balance (&rest args)
  "Команда БАЛАНС: вывести баланс счёта в банке."
  (declare (ignore args))
  (format t "На вашем счёте в банке ~a монет.~%" (balance (name *player*))))

(defun command-transfer (&rest person-and-sum-args)
  "Команда ПЕРЕВЕСТИ: перевести деньги на счёт другого игрока"
  (if (= 2 (length person-and-sum-args))
      (let ((person (string-capitalize (first person-and-sum-args)))
            (sum (handler-case (parse-integer (second person-and-sum-args))
                   (parse-error () -1))))
        (if (plusp sum)
            (if (<= sum (balance *player*))
                (if (user-exists-p person)
                    (transfer *player* person sum)
                    (format t "Нет игрока с именем ~A.~%Укажите имя получателя полностью.~%" person))
                (format t "У вас нет таких денег. Надо быть экономнее"))
            (format t "Да введите же число по-человечески. Число болше нуля, что непонятного?~%")))
      (wrong-command t "ПЕРЕВЕСТИ" "ИМЯ" "ЧИСЛО")))
  

(defun command-mail (&rest subcommand-and-options)
  "Команда ПОЧТА: получить или написать письмо"
  (flet ((mail-usage () 
	   (format t "И что делать? Используйте команду ПОЧТА так:~%почта получить~%почта писать <имя>~%~%")))
    (if (zerop (length subcommand-and-options))
	(mail-usage)
	(destructuring-bind (subcommand &rest options) subcommand-and-options
	  (word-dispatch subcommand
	    ("писать"
	     (if (/= 1 (length options))
		 (format t "Укажите имя получателя.~%")
		 (let ((receiver-name (first options)))
		   (if (user-exists-p receiver-name)
		       (let ((mail-editor
			      (make-instance 'text-editor-fsm
					     :after-editing-cb
					     (lambda (text) (send-mail (name *player*) receiver-name text)))))
			 (push-input-handler
			  (lambda (client input) (declare (ignore client)) (process-input1 mail-editor input))))
		       (format t "Игрока с именем \"~a\" не существует.~%" receiver-name)))))
	    ("получить"
	     (if (plusp (length options))
		 (format t "подкоманда получить не имеет аргументов.~%")
		 (deliver-mail-for *player* *player-room*)))
	    (t (mail-usage)))))))

(defun command-read (&rest items)
  (pvalue items)
  (if (/= (length items) 1)
      (format t "Вы можете за раз прочесть один текст или сообщение.~%")
      (let ((item (find-in-inventory (first items))))
	(pvalue item (class-of item))
	(if (and item (eql (class-of item) (find-class 'letter)))
	    (format t "Содержание:~%~A~%" (text item))
	    (format t "Вы порылись в инвентаре, но не смогли прочитать ничего, похожего на ~A.~%" (first items))))))

(defun command-list (&rest ignored)
  "Вывести список товаров в магазине"
  (declare (ignore ignored))
  (if (eq (class-of *player-room*) (find-class 'shop-room))
      (progn
        (format t "    ЦЕНА  НАЗВАНИЕ~%")
        (iter (for item in (price-list *player-room*))
              (format t "~8D  ~A~%" (price item) (name item)))
        (terpri))
      (format t "Вы не в магазине.~%")))

(defun command-buy (&rest items)
  (pvalue items)
  (if (eq (class-of *player-room*) (find-class 'shop-room))
      (if (= (length items) 1)
	  (let ((buy-item (find (first items) (price-list *player-room*) 
                                :key #'name :test #'mudname-equal)))
	    (if buy-item
		(if (<= (price buy-item) (money *player*))
		    (push (copy-obj buy-item) (inventory *player*))
		    (format t "У вас недостаточно денег.~%"))
		(format t "Такая вещь здесь не продаётся.~%")))
	  (format t "Можно купить только одну вещь за раз.~%"))
      (format t "Вы не в магазине.~%")))
		
(defun command-list-commands ()
  "Комманда для вывода списка других комманд"
  (format t
"Команды:
  СЕВЕР  -- идти на север
  ЮГ     -- идти на юг
  ЗАПАД  -- идти на запад
  ВОСТОК -- идти на восток
  ВЫХОДЫ -- перечислить выходы из комнаты
  ОГЛЯДЕТЬСЯ -- напечатать подробное описание комнаты
  КАРТА -- напечатать карту зоны
Работа с вещами:
  ВЗЯТЬ ВЕЩЬ -- взять ВЕЩЬ с пола
  ИНВЕНТАРЬ -- напечатать ваши вещи
  СКЛАД -- управляет складом
Банк:
  ВЛОЖИТЬ ЧИСЛО -- вложить в банк определённое количество денег
  СНЯТЬ ЧИСЛО -- снять со счёта в банке определённое количество денег
  БАЛАНС -- узнать баланс своего счёта
Общение
  ГОВОРИТЬ: ваши слова услышат все в комнате
  БОЛТАТЬ: ваши слова услышат все игроки в игре
  ПОЧТА: писать и получать письма

  КОНЕЦ -- выйти из игры~%~%"))

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
     ("вложить" command-deposit)
     ("снять" command-withdraw)
     ("баланс" command-balance)
     ("перевести" command-transfer)
     ("почта" command-mail)
     ("читать" command-read)
     ("команды" ,'command-list-commands)
     ("конец"  ,'command-leave))))
