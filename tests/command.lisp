(in-package :cl-user)
(defpackage :lispmud/tests/command
  (:use :cl)
  (:use :lispmud/core-globalvars)
  (:use :lispmud/core-threadvars)
  (:import-from :alexandria #:set-equal)
  (:import-from :lispmud/tables #:recreate-all-tables)
  (:import-from :lispmud/player #:player #:output #:name)
  (:import-from :lispmud/command #:command-chat #:command-mail #:init-commands)
  (:import-from :lispmud/input-handlers #:terminal-input-handler)
  (:import-from :lispmud/core-mail #:recv-mail)
  (:import-from :lispmud/core-server #:client #:client-on-command #:out-stream
                #:globvars #:input-handlers #:push-input-handler)
  (:import-from :lispmud/core-zone #:zone)
  (:import-from :lispmud/userdb
                #:reset-online-users #:online-user-names #:try-set-user-online
                #:set-user-offline #:*online-players* #:online-players
                #:register-user))
(in-package :lispmud/tests/command)

(5am:def-suite command
  :in :lispmud
  :description "command test suite")

(5am:in-suite command)

(5am:test command-chat
  (let* ((*online-players* (make-hash-table :test 'equal :synchronized t))
         (player-frodo (make-instance 'player :name "Фродо" :output (make-string-output-stream)))
         (player-sam (make-instance 'player :name "Cэм" :output (make-string-output-stream)))
         (*player* player-frodo))
    ;; Mark user Foo as online.
    (try-set-user-online player-frodo)
    (try-set-user-online player-sam)
    (command-chat "привет," "пацаны")
    (5am:is (search "Фродо болтает: привет, пацаны" (get-output-stream-string (output player-frodo))))
    (5am:is (search "Фродо болтает: привет, пацаны" (get-output-stream-string (output player-sam))))))

(defun make-mocked-zone ()
  (list (make-instance 'zone :name "Mocked zone" :entry-rooms (list :mocked-room))))

#+(or)
(5am:test command-mail
  (let* ((*online-players* (make-hash-table :test 'equal :synchronized t))
         (player-frodo (make-instance 'player :name "Фродо" :output (make-string-output-stream)))
         (player-sam (make-instance 'player :name "Сэм" :output (make-string-output-stream)))
         (*player* player-frodo)
         (*db-connection-spec* (list "lispmudtest" "lispmudtest" "lispmudtest" "localhost"))
         (*zone-list* (make-mocked-zone))
         ;; separate let because constructor need dynamic vars
         (*client* (make-instance 'client :input-handlers nil
                                          :out-stream (make-string-output-stream))))
    (apply #'recreate-all-tables *db-connection-spec*)
    ;; client does not set *player* globvar in its constructor, set it with setf
    (setf (getf (globvars *client*) '*player*) *player*)
    (init-commands)
    (push-input-handler #'terminal-input-handler)
    (pomo:with-connection (append *db-connection-spec* '(:pooled-p t))
      (register-user (name player-frodo) "password")
      (register-user (name player-sam) "password")
      (try-set-user-online player-frodo)
      (try-set-user-online player-sam)
      (5am:is (= 1 (length (input-handlers *client*))))
      (client-on-command *client* nil (format nil "почта писать Сэм~%"))
      (5am:is (= 2 (length (input-handlers *client*))))
      ;(command-mail "писать" "Сэм")
      (client-on-command *client* nil (format nil "Привет,~%"))
      (client-on-command *client* nil (format nil "Сэм!.~%"))
      (client-on-command *client* nil (format nil "\\s~%"))
      (format t "~a" (get-output-stream-string (out-stream *client*)))
      (destructuring-bind (sender-name receiver-name mail-text)
          (first (recv-mail "Сэм"))
        (5am:is (equal "Фродо" sender-name))
        (5am:is (equal "Сэм" receiver-name))
        (5am:is (equal (format nil "Привет,~%Сэм!.~%") mail-text))))))
