(in-package :mudsketcher)

(defclass zone-canvas (canvas)
  ((zone :initarg :zone :accessor zone)
   (x-grid-step :accessor x-grid-step)
   (y-grid-step :accessor y-grid-step)))

(defparameter *myout* *standard-output*)

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

(defmethod render-item (canvas (room myroom) x y mode selected-p hover-p &key data)
  (declare (ignore canvas data mode))
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

(defun regenerate-exits (zone-canvas)
  (let* ((canvas zone-canvas)
	 (zone (zone canvas))
	 (w (default-width canvas))
	 (h (default-height canvas))
	 (map (lispmud::map-array zone))
	 (x-dim (array-dimension map 1))
	 (w (* x-dim (round w (* 2 x-dim)) 2))
	 (x-grid-step (/ w x-dim))
	 (y-dim (array-dimension map 0))
	 (h (* y-dim (round h (* 2 y-dim)) 2))
	 (y-grid-step (/ h y-dim))
	 #+nil	 bg-layer
	 exits-layer
	 #+nil	 rooms-layer)
    (flet ((x-coord (x) (* x-grid-step (+ 0.5 x)))
	   (y-coord (y) (* y-grid-step (+ 0.5 y))))

      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (let ((x x) (y y))
		    (when (aref map y x)
		      (if (lispmud::south-exit (aref map y x))
			  (push (make-canvas-item :x 0 :y 0 :draw-obj (exit-drawer (x-coord x) (y-coord y) (x-coord x) (y-coord (1+ y))))
				exits-layer))
		      (if (lispmud::east-exit (aref map y x))
			  (push (make-canvas-item :x 0 :y 0 :draw-obj (exit-drawer (x-coord x) (y-coord y) (x-coord (1+ x)) (y-coord y)))
				exits-layer))))))
      (setf (getf (layers zone-canvas) :exits-layer) exits-layer))))

(defun round-to (x step)
  (+ (/ step 2) (* step (round (- x (/ step 2)) step))))

(defun round-to-array (x step)
  (round (- x (/ step 2)) step))

(defun indexes-in-array (array element &key (test 'eql))
  (dotimes (y (array-dimension array 0))
    (dotimes (x (array-dimension array 1))
      (when (funcall test (aref array y x) element)
	(return-from indexes-in-array (list y x)))))
  nil)
	

(defmethod move-item (canvas (room myroom) x y)
  (unlink room)
  (regenerate-exits canvas)
  (let ((map (map-array (zone canvas)))
	(array-x (round-to-array x (x-grid-step canvas)))
	(array-y (round-to-array y (y-grid-step canvas))))
    (destructuring-bind (old-array-y old-array-x)
	(indexes-in-array map room)
      (if (not (aref map array-y array-x))
	  (progn
	    (setf (aref map old-array-y old-array-x) nil
		  (aref map array-y array-x) room
					;	      selected-room-x new-x
					;	      selected-room-y new-y
		  )
	    (list (round-to x (x-grid-step canvas))
		  (round-to y (y-grid-step canvas))))
	  (list nil nil)))))


(defmethod initialize-instance :after ((canvas zone-canvas) &key &allow-other-keys)
  (let* ((zone (zone canvas))
	 (w (default-width canvas))
	 (h (default-height canvas))
	 (map (lispmud::map-array zone))
	 (x-dim (array-dimension map 1))
	 (w (* x-dim (round w (* 2 x-dim)) 2))
	 (x-grid-step (/ w x-dim))
	 (y-dim (array-dimension map 0))
	 (h (* y-dim (round h (* 2 y-dim)) 2))
	 (y-grid-step (/ h y-dim))
	 bg-layer
	 exits-layer
	 rooms-layer)
    (setf (x-grid-step canvas) x-grid-step)
    (setf (y-grid-step canvas) y-grid-step)
    (flet ((x-coord (x) (* x-grid-step (+ 0.5 x)))
	   (y-coord (y) (* y-grid-step (+ 0.5 y))))
      (iter (for x from 0 below x-dim)
	    (push 
	     (make-canvas-item :x 0 :y 0 :draw-obj (grid-line-drawer (x-coord x) (y-coord 0) (x-coord x) (y-coord (1- y-dim))))
	     bg-layer))
      (iter (for y from 0 below y-dim)
	    (push 
	     (make-canvas-item :x 0 :y 0 :draw-obj (grid-line-drawer (x-coord 0) (y-coord y) (x-coord (1- x-dim)) (y-coord y)))
	     bg-layer))
      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (let ((x x) (y y))
		    (when (aref map y x)
		      (if (lispmud::south-exit (aref map y x))
			  (push (make-canvas-item :x 0 :y 0 :draw-obj (exit-drawer (x-coord x) (y-coord y) (x-coord x) (y-coord (1+ y))))
				exits-layer))
		      (if (lispmud::east-exit (aref map y x))
			  (push (make-canvas-item :x 0 :y 0 :draw-obj (exit-drawer (x-coord x) (y-coord y) (x-coord (1+ x)) (y-coord y)))
				exits-layer))))))
      (iter (for x from 0 below x-dim)
	    (iter (for y from 0 below y-dim)
		  (when (aref map y x)
		    (push (make-canvas-item :x (* (/ w x-dim) (+ 0.5 x)) :y (* (/ h y-dim) (+ 0.5 y))
					    :draw-obj (aref map y x))
			  rooms-layer))))
      (setf (layers canvas)
	    (list :bg-layer bg-layer :exits-layer exits-layer :rooms-layer rooms-layer)))))

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
                (drawing-area :var drawing-area)))
	(let ((canvas (make-instance 'zone-canvas :zone *edited-zone*
				     :default-width 200 :default-height 200)))
	  (connect-canvas-signals canvas drawing-area))
        (widget-show w)))))
