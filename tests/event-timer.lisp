(in-package :cl-user)
(defpackage :lispmud/tests/event-timer
  (:use :cl)
  (:use :mockingbird)
  (:import-from :alexandria #:appendf)
  (:import-from :lispmud/event-timer
                #:make-mud-event #:start-event-loop #:stop-event-loop #:*timer* #:*timer-period*
                #:*timer-stop-flag* #:pick-events #:add-event #:*new-scheduled-events*
                #:*scheduled-events*))
(in-package :lispmud/tests/event-timer)

(5am:def-suite event-timer
  :in :lispmud
  :description "test suite for :lispmud/event-timer package")

(5am:in-suite event-timer)

(5am:test mud-event-structure
  (5am:is (make-mud-event :time 1 :function (lambda () nil)))
  (5am:is (make-mud-event :time 1 :function (lambda () nil) :repeat-interval 10)))

(5am:test start-event-loop
  (sb-ext:without-package-locks
    (let ((*timer* nil)
          (*timer-stop-flag* nil))
      (with-dynamic-stubs ((sb-ext:schedule-timer t)
                           (sb-ext:unschedule-timer t)
                           (sb-ext:make-timer 'test-timer))
        (start-event-loop)
        (5am:is (eq nil *timer-stop-flag*))
        (5am:is (eq 'test-timer *timer*))
        (stop-event-loop)
        (5am:is (eq t *timer-stop-flag*))
        (pick-events)
        (5am:is (eq nil *timer-stop-flag*))))))

(5am:test event-loop
  (let ((*timer-period* 0.001))
    (setf *timer* nil) ; Not using let because special variable bindings are thread-local
    (setf *timer-stop-flag* nil)
    (unwind-protect
         (progn
           (start-event-loop)
           (5am:is (eq nil *timer-stop-flag*))
           (stop-event-loop)
           (5am:is (eq t *timer-stop-flag*))
           (sleep 0.1)
           (5am:is (not (sb-ext:timer-scheduled-p *timer*)))
           (5am:is (eq nil *timer-stop-flag*)))
      (sb-ext:unschedule-timer *timer*)
      (setf *timer-stop-flag* nil)
      (setf *timer* nil))))

(5am:test pick-events
  (let ((*new-scheduled-events* nil)
        (*scheduled-events* nil)
        (*timer-stop-flag* nil)
        (executed-events nil))
    (sb-ext:without-package-locks
      (with-dynamic-stubs ((get-internal-real-time (* 0 internal-time-units-per-second)))
        (add-event 30 (lambda () (appendf executed-events '(a))) nil nil)
        (add-event 40 (lambda () (appendf executed-events '(b))) nil 60)
        (add-event 150 (lambda () (appendf executed-events '(c))) nil nil)
        (add-event 20 (lambda () (appendf executed-events '(d))) nil 80))
      (5am:is (equal 4 (length *new-scheduled-events*)))
      (5am:is (equal nil *scheduled-events*))
      (with-dynamic-stubs ((get-internal-real-time (* 100 internal-time-units-per-second)))
        (pick-events))
      (5am:is (equal 2 (length *new-scheduled-events*)))
      (5am:is (equal '(d a b) executed-events))
      (5am:is (equal 1 (length *scheduled-events*)))
      (with-dynamic-stubs ((get-internal-real-time (* 100 internal-time-units-per-second)))
        (add-event 10 (lambda () (appendf executed-events '(e))) nil 120)
        (add-event 90 (lambda () (appendf executed-events '(f))) nil 130))
      (5am:is (equal 4 (length *new-scheduled-events*)))

      (setf executed-events nil)
      (with-dynamic-stubs ((get-internal-real-time (* 200 internal-time-units-per-second)))
        (pick-events))
      (5am:is (equal 4 (length *new-scheduled-events*)))
      (5am:is (equal '(e c b d f) executed-events))
      (5am:is (equal nil *scheduled-events*))
      (setf executed-events nil)
      (with-dynamic-stubs ((get-internal-real-time (* 300 internal-time-units-per-second)))
        (pick-events))
      (5am:is (equal 2 (length *new-scheduled-events*)))
      (5am:is (equal '(b d) executed-events))
      (5am:is (equal 2 (length *scheduled-events*)))

      (setf executed-events nil)
      (with-dynamic-stubs ((get-internal-real-time (* 400 internal-time-units-per-second)))
        (pick-events))
      (5am:is (equal 4 (length *new-scheduled-events*)))
      (5am:is (equal '(e f b d) executed-events))
      (5am:is (equal nil *scheduled-events*)))))
