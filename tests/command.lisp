(in-package :cl-user)
(defpackage :lispmud/tests/command
  (:use :cl)
  (:import-from :alexandria #:set-equal)
  (:import-from :lispmud/core-threadvars #:*player*)
  (:import-from :lispmud/player #:player #:output)
  (:import-from :lispmud/command #:command-chat)
  (:import-from :lispmud/userdb
                #:reset-online-users #:online-user-names #:try-set-user-online
                #:set-user-offline #:*online-players* #:online-players))
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
