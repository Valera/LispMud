(in-package :mudsketcher)

(defclass canvas ()
  ((layers :initform nil :accessor layers :initarg :layers)
   (last-click-time :initform -1000000 :accessor last-click-time)
   (active-item :initform nil :accessor active-item)))
; :
(defgeneric cairo-draw (canvas width height &optional context)
  (:method (c w h &optional (context *context*))
    (let ((*context* context))
      (iter (for layer in (rest (layers c)) by #'cddr)
	    (iter (for draw-plist in layer)
		  (destructuring-bind (&key x y draw-fun other) draw-plist
		    (when draw-fun
		      (funcall draw-fun x y w h))))))))

(defgeneric handle-button-press (canvas event width height)
  (:method ((c canvas) event w h)
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

(defgeneric handle-pointer-move (canvas event width height)
  (:method ((c canvas) ev w h)
    (let ((x (event-motion-x ev))
	  (y (event-motion-y ev))
	  (acitve-item (active-item c)))
      (if (member :button1-mask (event-motion-state ev))
	  (when (active-item c)
	    (destructuring-bind (&key on-drag)
		(active-item c)
	      (if on-drag
		  (funcall on-drag (active-item c)))))
	  ;; THEN: hover;
	  ()
	  #+nil		 (destructuring-bind (&key on-hover)
			     (active-item c)
			   (if on-hovet))
	  ))))

(defun grid-line-drawer (x0 y0 x1 y1)
  #'(lambda (x y width height)
      (declare (ignore x y width height))
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

(defun room-drawer (x y room)
  #'(lambda (x0 y0 w h)
      (declare (ignore x0 y0 w h))
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
	(stroke))))

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
		  (when (aref map y x)
		    (if (lispmud::south-exit (aref map y x))
			(push (list :x 0 :y 0 :draw-fun (exit-drawer (x-coord x) (y-coord y) (x-coord x) (y-coord (1+ y))))
			      exits-layer))
		    (if (lispmud::east-exit (aref map y x))
			(push (list :x 0 :y 0 :draw-fun (exit-drawer (x-coord x) (y-coord y) (x-coord (1+ x)) (y-coord y)))
			      exits-layer)))))
      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (when (aref map y x)
;		    (set-source-color cl-colors:+grey55+)
		    #+nil		    (when (< (hypot (- x-mouse (x-coord x)) (- y-mouse (y-coord y))) 20)
					      (draw-room-selection (x-coord x) (y-coord y)))
;		    (set-source-color cl-colors:+grey25+)
		    #+nil		    (when (and (eql *selected-room* (aref map y x)))
					      (draw-room-selection (x-coord x) (y-coord y)))
		    (push (list :x 0 :y 0 :draw-fun (room-drawer (* (/ w x-dim) (+ 0.5 x)) (* (/ h y-dim) (+ 0.5 y)) (aref map y x))); :data (aref map y x))
			  rooms-layer))))
      (list :bg-layer bg-layer :exits-layer exits-layer :rooms-layer rooms-layer))))

(defun run-canvas-example ()
  (within-main-loop
    (let ( #+ nil (cb-list (make-instance 'array-list-store)))
      #+nil      (store-add-column cb-list gobject:+g-type-string+ #'cairo-fn-name)
      #+nil      (iter (for fn in (get-draw-fns))
		       (store-add-item cb-list fn))
      (let-ui (gtk-window
               :var w
               :default-width 300
               :default-height 400
               :type :toplevel
               :title "Cairo drawing"
               (v-box
					;                (combo-box :var combo :model cb-list) :expand nil
                (cairo-w :var cw)))
	(let ((canvas (make-instance 'canvas :layers (make-layers *edited-zone* 200 200))))
	  (setf (cairo-w-draw-fn cw)
		#'(lambda (w h)
;		    (with-gdk-context (ctx (widget-window cw))
;		      (with-context (ctx)
			(cairo-draw canvas w h))));))

					;        (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
					;         (cell-layout-pack-start combo renderer)
					;        (cell-layout-add-attribute combo renderer "text" 0))
	#+nil        (gobject:connect-signal combo "changed"
					     (lambda (widget)
					       (declare (ignore widget))
					       (let ((iter (combo-box-active-iter combo)))
						 (when iter
						   (setf (cairo-w-draw-fn cw)
							 (cairo-fn-fn (tree-model-item cb-list iter)))))))
	#+nil        (setf (combo-box-active-iter combo) (tree-model-iter-first cb-list))
        (widget-show w)))))