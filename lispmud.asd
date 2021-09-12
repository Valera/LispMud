;;; lispmud.asd
(defsystem :lispmud
  :serial t
  :depends-on (#:split-sequence #:alexandria #:iterate #:sb-queue #:postmodern
                                #:bordeaux-threads #:usocket #:cl-store
                                #:metabang-bind #:fiveam)
  :around-compile (lambda (next)
                    (with-compilation-unit (:policy '(optimize (debug 3) (safety 3) (speed 0))) (funcall next)))
  :components ((:module "src"
			:serial t
			:components ((:file "core-globalvars")
				     (:file "core-utils")
                                     (:file "core-db")
				     (:file "core-mail")
				     (:file "rucase")
				     (:file "color-codes")
				     (:file "event-timer")
				     (:file "core-threadvars")
				     (:file "core-command")
				     (:file "core-FSM")
				     (:file "core-streams")
                                     (:file "tables")
				     (:file "player")
				     (:file "trigger")
				     (:file "store")
				     (:file "userdb")
				     (:file "bank")
				     (:file "exchange")
				     (:file "roll")
				     (:file "registration")
				     (:file "core-room")
				     (:file "shop")
				     (:file "core-items")
				     (:file "mob")
				     (:file "core-zone")
				     (:file "world")
;				     (:file "core-telnet")
				     (:file "core-server")
				     (:file "input-handlers")
				     (:file "text-editor")
				     (:file "command")
				     (:file "main")
                                     (:file "package"))))
  :in-order-to ((test-op (test-op "lispmud/tests"))))

(defsystem "lispmud/tests"
  :author "Valeriy Fedotov"
  :license "LLGPL"
  :depends-on ("lispmud"
               "fiveam"
               "mockingbird")
  :components ((:module "tests"
                :components
                ((:file "core-command")
                 (:file "userdb")
                 (:file "text-editor")
                 (:file "command")
                 (:file "event-timer"))))
  :description "Test system for lispmud"
  :perform (test-op (op c) (symbol-call :5am :run! :lispmud)))
