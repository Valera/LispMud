(in-package :mudsketcher)

(defclass canvas ()
  ((drawing-area :accessor drawing-area)
   (layers :initform nil :accessor layers :initarg :layers)
   (active-item :initform nil :accessor active-item)
   (hover-item :initform nil :accessor hover-item)
   (default-width :initarg :default-width :accessor default-width)
   (default-height :initarg :default-height :accessor default-height)
   (scaling :initform 1.0 :accessor scaling)
   (drag-start-pos :initform nil :accessor drag-start-pos)
   (drag-mode-p :initform nil :accessor drag-mode-p)
   (select-cb :initform nil :initarg :select-cb :accessor select-cb)
   (unselect-cb :initform nil :initarg :unselect-cb :accessor unselect-cb)))

(defstruct canvas-item 
  x y draw-obj)

(defgeneric render-item (canvas item x y mode selected-p hover-p &key data))
(defgeneric move-item (canvas imem x y))
(defgeneric bound-rect (item))
(defgeneric full-update (canvas))
(defgeneric handle-double-click (canvas x y))

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
		      (otherwise (render-item c draw-obj x y :normal
					      (eql (active-item c) item) (eql (hover-item c) item))))))))))

(defgeneric handle-button-press (canvas event width height)
  (:method ((c canvas) event w h)
    (let* ((scale (scaling c))
	   (click-x (/ (event-button-x event) scale))
	   (click-y (/ (event-button-y event) scale)))
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
		      (if (and (active-item c) (unselect-cb c))
			  (funcall (unselect-cb c) (canvas-item-draw-obj (active-item c))))
		      (setf (active-item c) item)
		      (if (select-cb c)
			  (funcall (select-cb c) draw-obj))
		      (setf (drag-start-pos c) (list click-x click-y))
		      (return-from handle-button-press item)))))
      (when (and (active-item c) (unselect-cb c))
	(funcall (unselect-cb c) (canvas-item-draw-obj (active-item c))))
      (setf (active-item c) nil)
      (if (eql :2button-press (event-button-type event))
	  (handle-double-click c click-x click-y)
	  (setf (drag-start-pos c) (list click-x click-y)))
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
		  (move-item c (canvas-item-draw-obj active) pointer-x pointer-y)
		(if (and x y)
		    (setf (canvas-item-x active) x
			  (canvas-item-y active) y)))))
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

(defvar *draw-mode* :normal)

(defun connect-canvas-signals (canvas drawing-area &key (draw-mode :normal))
  (setf (drawing-area canvas) drawing-area)
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
			    (let ((*draw-mode* draw-mode))
			      (cairo-draw canvas w h ctx)
			      nil))))))
  (connect-signal drawing-area "button_press_event"
		  #'(lambda (widget event)
		      (handle-button-press canvas event 200 200)
		      (widget-queue-draw widget)))
  (connect-signal drawing-area "motion-notify-event"
		  #'(lambda (widget event &rest args)
		      (declare (ignore args))
		      (handle-pointer-move canvas event 200 200)
		      (widget-queue-draw widget))))
