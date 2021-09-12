;;; event-timer.lisp

;;; 

(in-package :cl-user)
(defpackage :lispmud/event-timer
  (:use :cl)
  (:import-from :bt #:with-lock-held #:make-lock)
  (:import-from :iter #:iter #:for #:while))
(in-package :lispmud/event-timer)

;; TODO: move this global variables to slots of a new class
(defvar *timer* nil "SBCL timer object used for periodical wakeups and execution of timed events")
(defvar *timer-period* 0.2 "Period of *timer* object")
(defvar *timer-stop-flag* nil "If this flag is not nil, timer will be unscheduled at the next activation")

(defvar *scheduled-events* nil "Sorted list of scheduled events for future execution")
(defvar *new-scheduled-events* nil "Events scheduled after last timer activation, not yet sorted")
(defvar *event-timer-lock* (make-lock "Event timer system global lock") "Lock for *timer* object and *timer-stop-flag*")
(defvar *new-scheduled-events-lock* (make-lock "*new-scheduled-events* list lock") "Lock for *new-scheduled-events* list")

(defun %sort-and-merge-new-scheduled-events (new-scheduled-events scheduled-events)
  "Adds elemets of new-sheduled-events keeping time ascending. sheduled-events must be sorted."
  (let ((sorted-new-scheduled-events (sort new-scheduled-events #'< :key #'mud-event-time)))
    (merge 'list sorted-new-scheduled-events scheduled-events #'< :key #'mud-event-time)))

(defun pick-events ()
  "Proccess incoming request to schedule events, executes events that are ready,
reschedules events if necessary.

If *timer-stop-flag* is non-nil, stops event loop. Otherwise sorts *new-scheduled-events* and merges it
to *scheduled-events* keeping time ascending. After that executes all events that are due to current time,
reschedules events that have repeat-interval.

Note that even if repeat-interval of event is less than *timer-period*, each event will be executed
only once per pick-events. For good matching of repeat-interval and real time repeat time
you should set repeat-interval to be 4-5 times more than *timer-period*."
  (with-lock-held (*event-timer-lock*)
    (if *timer-stop-flag*
	(progn (sb-ext:unschedule-timer *timer*)
	       (setf *timer-stop-flag* nil))
	(progn
	  (with-lock-held (*new-scheduled-events-lock*)
            (setf *scheduled-events* (%sort-and-merge-new-scheduled-events
                                      *new-scheduled-events* *scheduled-events*))
	    (setf *new-scheduled-events* nil))
          ;; Execute events from *scheduled-events* while their time is before current time.
	  (iter (while (and *scheduled-events*
			    (< (mud-event-time (first *scheduled-events*)) (get-internal-real-time))))
                (for event = (pop *scheduled-events*))
                (for event-function = (mud-event-function event))
                (when event-function
                  (format t "~&Calling fun ~a~%" event-function)
                  (funcall event-function)) ; FIXME: send message to worker thread. Now race conditions han happen.
                (when (mud-event-repeat-interval event)
                  (format t "~&...rescheduling it.~%")
                  (with-lock-held (*new-scheduled-events-lock*)
                    (setf (mud-event-time event) (+ (get-internal-real-time) (mud-event-repeat-interval event)))
                    (push event *new-scheduled-events*))))))))

(defstruct mud-event
  "Structure that holds description of event scheduled for future execution"
  (time (error "time slot does not have default value") :type integer)
  (function (error "function slot does not have default value") :type function)
  domain
  (repeat-interval nil  :type (or null integer)))

(defun add-event (time event-fun zone repeat-interval)
  "Add event for future execution. Events are processed at timer activations with *timer-period* period.
repeat-interval argument should be 4-5 times more than *timer-period* for more accurate scheduling.
If repeat-interval is less than *timer-perioud* event will be still executed only once per timer activation."
  (with-lock-held (*new-scheduled-events-lock*)
    (flet ((to-internal-units (seconds) (round (* seconds internal-time-units-per-second))))
      (push (make-mud-event :time (+ (get-internal-real-time) (to-internal-units time))
                            :function event-fun
                            :domain zone
                            :repeat-interval (when repeat-interval (to-internal-units repeat-interval)))
            *new-scheduled-events*))))

(defun start-event-loop ()
  "Starts event loop, calling function pick-events wiht *timer-period* period.
If timer was already started, it is stopped and all previous events are removed."
  (with-lock-held (*event-timer-lock*)
    (when *timer*
      (sb-ext:unschedule-timer *timer*))
    (with-lock-held (*new-scheduled-events-lock*)
      (setf *scheduled-events* nil
	    *new-scheduled-events* nil))
    (sb-ext:schedule-timer (setf *timer* (sb-ext:make-timer (lambda () (pick-events)) :thread t))
		    *timer-period* :repeat-interval *timer-period*)))

(defun stop-event-loop ()
  "Set timer stop flag. Timer will be actually stopped at its next activation."
  (with-lock-held (*event-timer-lock*)
    (setf *timer-stop-flag* t)))
