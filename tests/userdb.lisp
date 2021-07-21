(in-package :cl-user)
(defpackage :lispmud/tests/userdb
  (:use :cl)
  (:import-from :alexandria #:set-equal)
  (:import-from :lispmud/player #:player)
  (:import-from :lispmud/userdb
                #:reset-online-users #:online-user-names #:try-set-user-online
                #:set-user-offline #:*online-players*))
(in-package :lispmud/tests/userdb)

(5am:def-suite userdb
  :in :lispmud
  :description "command test suite")

(5am:in-suite userdb)

(5am:test online-users *online-players* (make-hash-table :test 'equal :synchronized t)
  (let ((*online-players* (make-hash-table :test 'equal :synchronized t))
        (player-foo (make-instance 'player :name "Foo"))
        (player-bar (make-instance 'player :name "Bar")))
    (5am:is (equal nil (online-user-names)))
    ;; Mark user Foo as online.
    (5am:is (try-set-user-online player-foo))
    (5am:is (equal '("Foo") (online-user-names)))
    ;; Mark user Foo as online, function should return false value.
    (5am:is (not (try-set-user-online player-foo)))
    (5am:is (equal '("Foo") (online-user-names)))
    ;; Mark user Bar as online.
    (5am:is (try-set-user-online player-bar))
    (5am:is (set-equal '("Foo" "Bar") (online-user-names)))
    ;; Mark user Foo as offline.
    (set-user-offline player-foo)
    (5am:is (equal '("Bar") (online-user-names)))
    ;; Reset online users.
    (reset-online-users)
    (5am:is (equal nil (online-user-names)))))