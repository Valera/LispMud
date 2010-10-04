(in-package :mudsketcher)

(defclass canvas ()
  ((layers :initform nil :accessor layers)
   (last-click-time :initform -1000000 :accessor last-click-time)
   (active-item :initform nil :accessor active-item)))
; :
(defgeneric cairo-draw (canvas width height &optional context)
  (:method cairo-draw ((c canvas) w h &optional context)
	   (let ((*context* context))
	     (iter (for layer in (rest (layers c)) by #'cddr)
		   (iter (for draw-plist in layer)
			 (destructuring-bind (&key x y draw-fun) draw-plist
			   (if draw-fun
			       (funcall draw-fun x y w h))))))))

(defgeneric handle-button-press (canvas event width height)
  (:method handle-button-press ((c canvas) event w h)
					;	   (flet ((within-bound-rect (bound-rect x y)
					;		    (destructuring-bind (xr yr wr hr) bound-rect
					;		      (and (< xr x (+ xr wr))
					;			   (< yr y (+ yr hr))))))
	   (let* ((click-x (event-button-x event))
		  (click-y (event-button-y event))
		  (click-time (event-button-time event))
		  (callback-type (if (< (- click-time (last-click-time c)) 1000) :on-2click :on-click)))
	     (iter (for layer in (reverse (layers c)) by #'cddr)
		   (iter (for draw-plist in layer)
			 (destructuring-bind (&key x y ((:bound-rect (rx ry rw rh))) clickable on-click on-2click)
			     draw-plist
			   (when (and clickable
				      (< (+ x rx) click-x (+ x rx rw))
				      (< (+ y ry) click-y (+ y ry rh)))
			     (setf (active-item c) draw-plist)
			     (ecase callback-type
			       (:on-2click (funcall on-2click draw-plist)) ; Double click.
			       (:on-click  (funcall on-click draw-plist))) ; Single click.
			     (return-from handle-button-press draw-plist)))))
	     nil)))


