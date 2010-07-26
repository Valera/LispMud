;;; event-timer.lisp

;;; 

(in-package :lispmud)

(defvar *timer*)
(defvar *timer-period* 0.2)
(defvar *timer-stop-flag* nil)

(defvar *scheduled-events*)
(defvar *new-scheduled-events*)
(defvar *new-scheduled-events-lock* (make-lock "Sheduled events list lock"))

(defun pick-events ()
  (if *timer-stop-flag*
      (progn (unschedule-timer *timer*)
	     (setf *timer-stop-flag* nil))
      (progn
	(flet ((earlier-event-p (x y) (< (getf x :time) (getf y :time))))
	  (let (new-scheduled-events)
	    (with-lock-held (*new-scheduled-events-lock*)
	      (shiftf new-scheduled-events *new-scheduled-events* nil))
	    (setf new-scheduled-events (sort new-scheduled-events #'earlier-event-p))
	    (setf *scheduled-events* (merge 'list new-scheduled-events *scheduled-events* #'earlier-event-p))))
	(iter (while (and *scheduled-events*
			  (< (getf (first *scheduled-events*) :time) (get-internal-real-time))))
	      (for event = (pop *scheduled-events*))
	      (when (getf event :function)
		(format t "calling fun ~a~%" (getf event :function))
		(funcall (getf event :function)))
	      (when (getf event :repeat-interval)
		(format t "...rescheduling it.")
		(with-lock-held (*new-scheduled-events-lock*)
		  (setf (getf event :time) (+ (get-internal-real-time) (getf event :repeat-interval)))
		  (push event *new-scheduled-events*)))))))

(defun add-event (time event-fun zone repeat-interval)
  (with-lock-held (*new-scheduled-events-lock*)
    (push (list :time (+ (get-internal-real-time) time) :function event-fun
		:domain zone :repeat-interval repeat-interval)
	  *new-scheduled-events*)))

;; LOCK!!!
(defun start-event-loop ()
  (setf *scheduled-events* nil
	*new-scheduled-events* nil)
  (schedule-timer (setf *timer* (make-timer (lambda ()
					      (format t "Timer time...~%")
					      (pick-events))))
		  *timer-period* :repeat-interval *timer-period*))

(defun stop-event-loop ()
  (setf *timer-stop-flag* t))

