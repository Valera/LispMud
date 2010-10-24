(in-package :mudsketcher)

(defclass canvas ()
  ((layers :initform nil :accessor layers :initarg :layers)
   (last-click-time :initform -1000000 :accessor last-click-time)
   (active-item :initform nil :accessor active-item)
   (hover-item :initform nil :accessor hover-item)
   (default-width :initarg :default-width :accessor default-width)
   (default-height :initarg :default-height :accessor default-height)
   (scaling :initform 1.0 :accessor scaling)
   (drag-start-pos :initform nil :accessor drag-start-pos)
   (drag-mode-p :initform nil :accessor drag-mode-p)))

(defstruct canvas-item 
  x y draw-obj)

(defgeneric render-item (item x y mode selected-p hover-p &key data))
(defgeneric move-item (imem x y))
(defgeneric bound-rect (item))

(defgeneric cairo-draw (canvas width height &optional context)
  (:method (c w h &optional (context *context*))
    (let ((*context* context))
      (setf (scaling c) (min (/ w (default-width c)) (/ h (default-height c))))
      (scale (scaling c) (scaling c))
      (iter (for layer in (rest (layers c)) by #'cddr)
	    (iter (for item in layer)
		  (let ((draw-obj (canvas-item-draw-obj item))
			(x (canvas-item-x item))
			(y (canvas-item-y item)))
		    (typecase draw-obj
		      (function (funcall draw-obj x y w h))
		      (otherwise (render-item draw-obj x y :normal
					      (eql (active-item c) item) (eql (hover-item c) item))))))))))

(defgeneric handle-button-press (canvas event width height)
  (:method ((c canvas) event w h)
    (let* ((scale (scaling c))
	   (click-x (/ (event-button-x event) scale))
	   (click-y (/ (event-button-y event) scale))
	   #+nil	   (click-time (event-button-time event))
	   #+nil	   (callback-type (if (< (- click-time (last-click-time c)) 1000) :on-2click :on-click)))
      (setf (drag-mode-p c) nil)
      (iter (for layer in (reverse (layers c)) by #'cddr)
	    (iter (for item in layer)
		  (let ((draw-obj (canvas-item-draw-obj item))
			(x (canvas-item-x item))
			(y (canvas-item-y item)))
		    (when (and (not (functionp draw-obj))
			       (destructuring-bind (br-x br-y br-w br-h) (bound-rect draw-obj)
				 (and (< (+ x br-x) click-x (+ x br-x br-w))
				      (< (+ y br-y) click-y (+ y br-y br-h)))))
		      (setf (active-item c) item)
		      (setf (drag-start-pos c) (list click-x click-y))
		      #+nil		      (ecase callback-type
						(:on-2click (funcall on-2click draw-plist)) ; Double click.
						(:on-click  (funcall on-click draw-plist))) ; Single click.
		      (return-from handle-button-press item)))))
      (setf (active-item c) nil)
      (setf (drag-start-pos c) (list click-x click-y))
      nil)))

(defgeneric handle-pointer-move (canvas event width height)
  (:method ((c canvas) event w h)
    (let* ((scale (scaling c))
	   (pointer-x (/ (event-motion-x event) scale))
	   (pointer-y (/ (event-motion-y event) scale)))
      (if (member :button1-mask (event-motion-state event))
	  ;; THEN: Trying to drag something.
	  (with-accessors ((active active-item) (dsp drag-start-pos) (drag-mode-p drag-mode-p)) c
	    (when (and active 
		       (or drag-mode-p
			   (and dsp
				(< 10 (hypot (- pointer-x (first dsp))
					     (- pointer-y (second dsp)))))))
	      (setf drag-mode-p t)
	      (destructuring-bind (x y)
		  (move-item (canvas-item-draw-obj active) pointer-x pointer-y)
		(setf (canvas-item-x active) x
		      (canvas-item-y active) y))))
	  ;; THEN: Trying to hover over an item.
	  (progn
	    (setf (drag-mode-p c) nil)
	    (iter (for layer in (reverse (layers c)) by #'cddr)
		  (iter (for item in layer)
			(let ((draw-obj (canvas-item-draw-obj item))
			      (x (canvas-item-x item))
			      (y (canvas-item-y item)))
			  (when (and (not (functionp draw-obj))
				     (destructuring-bind (br-x br-y br-w br-h) (bound-rect draw-obj)
				       (and (< (+ x br-x) pointer-x (+ x br-x br-w))
					    (< (+ y br-y) pointer-y (+ y br-y br-h)))))
			    (setf (hover-item c) item)
			    (return-from handle-pointer-move item)))))
	    (setf (hover-item c) nil))))))

(defun connect-canvas-signals (canvas drawing-area)
  (connect-signal drawing-area "realize"
		  #'(lambda (widget)
		      (declare (ignore widget))
		      (pushnew :pointer-motion-mask (gdk-window-events (widget-window drawing-area)))
		      (pushnew :button-press-mask (gdk-window-events (widget-window drawing-area)))))
  (connect-signal drawing-area "configure-event"
			  #'(lambda (widget event)
			      (declare (ignore event))
			      (widget-queue-draw widget)))
  (connect-signal drawing-area "expose-event"
			  #'(lambda (widget event)
			      (declare (ignore event))
			      (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
				(with-gdk-context (ctx (widget-window widget))
				  (with-context (ctx)
				    (cairo-draw canvas w h ctx)
				    nil)))))
  (connect-signal drawing-area "button_press_event"
		  #'(lambda (widget event)
		      (handle-button-press canvas event 200 200)
		      (widget-queue-draw widget)))
  (connect-signal drawing-area "motion-notify-event"
		  #'(lambda (widget event &rest args)
		      (declare (ignore args))
		      (handle-pointer-move canvas event 200 200)
		      (widget-queue-draw widget))))
