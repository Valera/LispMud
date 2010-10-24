(in-package :mudsketcher)

(defclass canvas ()
  ((layers :initform nil :accessor layers :initarg :layers)
   (last-click-time :initform -1000000 :accessor last-click-time)
   (active-item :initform nil :accessor active-item)
   (hover-item :initform nil :accessor hover-item)))

(defgeneric render-item (item x y mode selected-p hover-p &key data))
(defgeneric move-item (imem x y))
(defgeneric bound-rect (item))

(defparameter *myout* *standard-output*)

(defgeneric cairo-draw (canvas width height &optional context)
  (:method (c w h &optional (context *context*))
    (let ((*context* context))
      (iter (for layer in (rest (layers c)) by #'cddr)
	    (iter (for draw-plist in layer)
		  (destructuring-bind (&key x y draw-fun) draw-plist
		    (typecase draw-fun
		      (function (funcall draw-fun x y w h))
		      (otherwise (render-item draw-fun x y :normal
					      (eql (active-item c) draw-plist) (eql (hover-item c) draw-plist))))))))))

(defgeneric handle-button-press (canvas event width height)
  (:method ((c canvas) event w h)
    (let* ((click-x (event-button-x event))
	   (click-y (event-button-y event))
#+nil	   (click-time (event-button-time event))
#+nil	   (callback-type (if (< (- click-time (last-click-time c)) 1000) :on-2click :on-click)))
      (iter (for layer in (reverse (layers c)) by #'cddr)
	    (iter (for draw-plist in layer)
		  (destructuring-bind (&key x y draw-fun) ;((:bound-rect (rx ry rw rh))) clickable on-click on-2click)
		      draw-plist
		    (when (and (not (functionp draw-fun))
			       (destructuring-bind (br-x br-y br-w br-h) (bound-rect draw-fun)
				 (and (< (+ x br-x) click-x (+ x br-x br-w))
				      (< (+ y br-y) click-y (+ y br-y br-h)))))
		      (setf (active-item c) draw-plist)
#+nil		      (ecase callback-type
			(:on-2click (funcall on-2click draw-plist)) ; Double click.
			(:on-click  (funcall on-click draw-plist))) ; Single click.
		      (return-from handle-button-press draw-plist)))))
      (setf (active-item c) nil)
      nil)))

(defgeneric handle-pointer-move (canvas event width height)
  (:method ((c canvas) ev w h)
    (let ((pointer-x (event-motion-x ev))
	  (pointer-y (event-motion-y ev)))
      (if (member :button1-mask (event-motion-state ev))
	  ;; THEN: Trying to drag something.
	  (when (active-item c)
	    (destructuring-bind (&key on-drag)
		(active-item c)
	      (if on-drag
		  (funcall on-drag (active-item c)))))
	  ;; THEN: Trying to hover over an item.
	  (progn
	    (iter (for layer in (reverse (layers c)) by #'cddr)
		(iter (for draw-plist in layer)
		      (destructuring-bind (&key x y draw-fun) ;((:bound-rect (rx ry rw rh))) clickable on-click on-2click)
			  draw-plist
			(when (and (not (functionp draw-fun))
				   (destructuring-bind (br-x br-y br-w br-h) (bound-rect draw-fun)
				     (and (< (+ x br-x) pointer-x (+ x br-x br-w))
					  (< (+ y br-y) pointer-y (+ y br-y br-h)))))
			  (setf (hover-item c) draw-plist)
			  #+nil		      (ecase callback-type
						(:on-2click (funcall on-2click draw-plist)) ; Double click.
						(:on-click  (funcall on-click draw-plist))) ; Single click.
			  (return-from handle-pointer-move draw-plist)))))
	    (setf (hover-item c) nil))))))

(defun grid-line-drawer (x0 y0 x1 y1)
  #'(lambda (x y w h)
      (declare (ignore x y w h))
      (set-source-color cl-colors:+grey80+)
      (move-to x0 y0)
      (line-to x1 y1)
      (set-source-color cl-colors:+grey80+)
      (stroke)))

(defun exit-drawer (x0 y0 x1 y1)
  #'(lambda (x y w h)
      (declare (ignore x y w h))
      (set-line-width 2)
      (move-to x0 y0)
      (line-to x1 y1)
      (set-source-rgb 0 0 0)
      (stroke)))

(defmethod render-item ((room myroom) x y mode selected-p hover-p &key data)
  (declare (ignore data mode))
  (when hover-p
    (let ((room-selection-size 28))
      (set-source-color cl-colors:+grey80+)
      (rounded-rectangle (- x (/ room-selection-size 2)) (- y (/ room-selection-size 2))
			 room-selection-size room-selection-size 5)
      (fill-path)))
  (when selected-p
    (let ((room-selection-size 30))
      (set-source-color cl-colors:+grey25+)
      (rounded-rectangle (- x (/ room-selection-size 2)) (- y (/ room-selection-size 2))
			 room-selection-size room-selection-size 5)
      (fill-path)))
  (let ((fill-color (case (lispmud::place-type room)
		      (:forest cl-colors:+DARKGREEN+)
		      (:indoors cl-colors:+yellow+)
		      (:city cl-colors:+brown4+)
		      (otherwise cl-colors:+grey100+)))
	(room-size 20))
    (set-line-width 2)
    (rectangle (- x (/ room-size 2)) (- y (/ room-size 2))
	       room-size room-size)
    (set-source-color fill-color)
    (fill-preserve)
    (set-source-rgb 1 0 0)
    (stroke)))

(defmethod bound-rect ((room myroom))
  (list -12 -12 24 24))

(defun make-layers (zone w h)
  (let* ((map (lispmud::map-array zone))
	 (x-dim (array-dimension map 1))
	 (w (* x-dim (round w (* 2 x-dim)) 2))
	 (x-grid-step (/ w x-dim))
	 (y-dim (array-dimension map 0))
	 (h (* y-dim (round h (* 2 y-dim)) 2))
	 (y-grid-step (/ h y-dim))
	 bg-layer
	 exits-layer
	 rooms-layer)
    (flet ((x-coord (x) (* x-grid-step (+ 0.5 x)))
	   (y-coord (y) (* y-grid-step (+ 0.5 y))))
      (iter (for x from 0 below x-dim)
	    (push 
	     (list :x 0 :y 0 :draw-fun (grid-line-drawer (x-coord x) (y-coord 0) (x-coord x) (y-coord (1- y-dim))))
	     bg-layer))
      (iter (for y from 0 below y-dim)
	    (push 
	     (list :x 0 :y 0 :draw-fun (grid-line-drawer (x-coord 0) (y-coord y) (x-coord (1- x-dim)) (y-coord y)))
	     bg-layer))
      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (let ((x x) (y y))
		    (when (aref map y x)
		      (if (lispmud::south-exit (aref map y x))
			  (push (list :x 0 :y 0 :draw-fun (exit-drawer (x-coord x) (y-coord y) (x-coord x) (y-coord (1+ y))))
				exits-layer))
		      (if (lispmud::east-exit (aref map y x))
			  (push (list :x 0 :y 0 :draw-fun (exit-drawer (x-coord x) (y-coord y) (x-coord (1+ x)) (y-coord y)))
				exits-layer))))))
      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (when (aref map y x)
		    (push (list :x (* (/ w x-dim) (+ 0.5 x)) :y (* (/ h y-dim) (+ 0.5 y))
				:draw-fun (aref map y x) :selected nil); :data (aref map y x))
			  rooms-layer))))
      (list :bg-layer bg-layer :exits-layer exits-layer :rooms-layer rooms-layer))))

(defun run-canvas-example ()
  (within-main-loop
    (let ()
      (let-ui (gtk-window
               :var w
               :default-width 300
               :default-height 400
               :type :toplevel
               :title "Cairo drawing"
               (v-box
                (cairo-w :var cw)))
	(let ((canvas (make-instance 'canvas :layers (make-layers *edited-zone* 200 200))))
	  (connect-signal cw "realize"
			  (lambda (widget)
			    (declare (ignore widget))
			    (pushnew :pointer-motion-mask (gdk-window-events (widget-window cw)))
			    (pushnew :button-press-mask (gdk-window-events (widget-window cw)))))
	  (connect-signal cw "button_press_event" #'(lambda (widget event)
						      (handle-button-press canvas event 200 200)
						      (widget-queue-draw widget)))
	  (connect-signal cw "motion-notify-event" #'(lambda (widget event &rest args)
						       (declare (ignore args))
						       (handle-pointer-move canvas event 200 200)
						       (widget-queue-draw widget)))
	  (setf (cairo-w-draw-fn cw)
		#'(lambda (w h)
		    (cairo-draw canvas w h))))
        (widget-show w)))))
