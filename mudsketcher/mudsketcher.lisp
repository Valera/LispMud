(in-package :mudsketcher)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cairo-w (drawing-area)
    ((draw-fn :initform 'draw-clock-face :accessor cairo-w-draw-fn))
    (:metaclass gobject:gobject-class)))

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :mudsketcher)))
(defparameter *edited-zone* (lispmud::load-zone (merge-pathnames "content/2.lzon" *src-location*)))
(defparameter *selected-room* nil)

(defstruct name-keyword name keyword)

(defun hypot (x y)
  (sqrt (+ (* x x) (* y y))))

(defun run-mudsketcher ()
  (within-main-loop
    (let* (file-name
	   ;; GTK variables
	   (builder (let ((builder (make-instance 'builder)))
                      (builder-add-from-file builder (namestring (merge-pathnames "mudsketcher/mudsketcher.ui" *src-location*)))
		      builder))
	   (window (builder-get-object builder "window1"))
;	   (new-dialog (builder-get-object "dialog1"))
	   (new-action (builder-get-object builder "new"))
	   (open-action (builder-get-object builder "open"))
	   (save-action (builder-get-object builder "save"))
	   (save-as-action (builder-get-object builder "save-as"))
	   (zone-settings-action (builder-get-object builder "zone-settings"))
	   (disconnect-action (builder-get-object builder "disconnect"))
	   (h-box (builder-get-object builder "hbox1"))
	   (entry (builder-get-object builder "entry1"))
	   (text-view (builder-get-object builder "textview1"))
	   (combo-box (builder-get-object builder "combobox1"))
	   (model (make-instance 'array-list-store))
	   (room-types '(:forest :indoors :city))
	   (room-type-names '("Лес" "Помещение" "Город"))
	   (canvas (make-instance 'zone-canvas :zone *edited-zone* :default-width 200 :default-height 200))
	   (drawing-area (make-instance 'drawing-area)))
      (labels ((debug-out (str)
		 (setf (text-buffer-text (text-view-buffer text-view)) str))
	       (initialize-new-dialog (d)
		 (let (;(entry (builder-get-object builder "zone-name-entry"))
		       (adj-y (builder-get-object builder "adjustment-y"))
		       (adj-x (builder-get-object builder "adjustment-x")))
		   (setf (adjustment-value adj-x) 6
			 (adjustment-value adj-y) 6))
		 (dialog-add-button d "gtk-cancel" :cancel)
		 (dialog-add-button d "gtk-new" :accept))
	       (new (&rest args)
		 (declare (ignore args))
		 (let ((d (builder-get-object builder "dialog1"))
		       (entry (builder-get-object builder "zone-name-entry"))
		       (adj-y (builder-get-object builder "adjustment-y"))
		       (adj-x (builder-get-object builder "adjustment-x")))
		   (widget-show d)
		   (when (eq :accept (dialog-run d))
		     (setf (zone canvas) (make-instance 'lispmud::zone :name (entry-text entry)
							:map-array (make-array (list (round (adjustment-value adj-y))
										     (round (adjustment-value adj-x)))
									       :adjustable t :initial-element nil)))
		     (full-update canvas))
		   (widget-hide d)))
	       (zone-settings (&rest args)
		 (declare (ignore args))
		 (let ((d (builder-get-object builder "dialog1"))
		       (entry (builder-get-object builder "zone-name-entry"))
		       (adj-y (builder-get-object builder "adjustment-y"))
		       (adj-x (builder-get-object builder "adjustment-x"))
		       (zone-map (lispmud::map-array *edited-zone*)))
		   (widget-show d)
		   (setf (adjustment-value adj-x) (array-dimension zone-map 1)
			 (adjustment-value adj-y) (array-dimension zone-map 0)
			 (entry-text entry) (lispmud::name *edited-zone*))
		   (when (eq :accept (dialog-run d))
		     (setf (name (zone canvas)) (entry-text entry)
			   (map-array (zone canvas)) (adjust-array zone-map (list (round (adjustment-value adj-y))
										  (round (adjustment-value adj-x)))
								   :initial-element nil))
		     (unlink-orphaned-rooms *edited-zone*)
		     (full-update canvas))
		   (widget-hide d)))
	       (cb-open (&rest args) (declare (ignore args))
                        (let ((d (make-instance 'file-chooser-dialog :action :open :title "Open file")))
                          (when file-name (setf (file-chooser-filename d) file-name))
                          (dialog-add-button d "gtk-cancel" :cancel)
                          (dialog-add-button d "gtk-open" :accept)
                          (when (eq :accept (dialog-run d))
                            (setf file-name (file-chooser-filename d)
				  (zone canvas) (load-zone file-name))
			    (full-update canvas))
			  (object-destroy d)))
               (save (&rest args) (declare (ignore args))
                     (if file-name
			 (save-zone (zone canvas) file-name)
                         (save-as)))
               (save-as (&rest args) (declare (ignore args))
                        (let ((d (make-instance 'file-chooser-dialog :action :save :title "Save file")))
                          (when file-name (setf (file-chooser-filename d) file-name))
                          (dialog-add-button d "gtk-cancel" :cancel)
                          (dialog-add-button d "gtk-save" :accept)
                          (if (eq :accept (dialog-run d))
                              (progn
                                (setf file-name (file-chooser-filename d))
                                (object-destroy d)
                                (save))
                              (object-destroy d))))
	       (initialize-model-and-combo-box (m c)
		 (store-add-column m "gchararray" #'identity)
		 (iter (for i in room-type-names)
		       (store-add-item m i))
		 (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
		   (cell-layout-pack-start c renderer :expand t)
		   (cell-layout-add-attribute c renderer "text" 0)))
	       (room-type-changed (c)
		 (declare (ignore c))
		 (let ((current-room (canvas-item-draw-obj (active-item canvas))))
		   (if current-room
		     (setf (place-type current-room) (nth (combo-box-active combo-box) room-types))))
		 (widget-queue-draw drawing-area))
	       (room-select (room)
		 (setf (combo-box-active combo-box) (position (place-type room) room-types))
		 (setf (entry-text entry) (LispMud::short-description room))
		 (setf (text-buffer-text (text-view-buffer text-view)) (description room)))
	       (room-unselect (room)
		 (setf (lispmud::short-description room) (entry-text entry)
		       (lispmud::description room) (text-buffer-text (text-view-buffer text-view))
		       (combo-box-active combo-box) (position (place-type room) room-types)
		       (entry-text entry) ""
		       (text-buffer-text (text-view-buffer text-view)) ""))
	       (disconnect-exits (&rest args)
		 (declare (ignore args))
		 (when (active-item canvas)
		   (let ((selected-room (canvas-item-draw-obj (active-item canvas)))
			 (zone-map (map-array (zone canvas))))
		     (destructuring-bind (selected-room-y selected-room-x)
			 (indexes-in-array zone-map selected-room)
		       (if (iter (for i in *exits*)
				 (if (exit selected-room i)
				     (leave t))
				 (finally nil))
			   (unlink selected-room)
			   (dolist (direction *exits*)
			     (let ((x1 (+ selected-room-x (dx-for-direction direction)))
				   (y1 (+ selected-room-y (dy-for-direction direction)))
				   room1)
			       (if (and (array-in-bounds-p zone-map y1 x1)
					(setf room1 (aref zone-map y1 x1)))
				   (setf (exit room1 (reverse-direction direction)) (make-instance 'exit :dest-room selected-room)
					 (exit selected-room direction) (make-instance 'exit :dest-room room1)))))))
		     (regenerate-exits canvas)
		     (widget-queue-draw drawing-area)))))
	(initialize-new-dialog (builder-get-object builder "dialog1"))
	(initialize-model-and-combo-box model combo-box)
	(setf (combo-box-model combo-box) model)
	(connect-signal combo-box "changed" #'room-type-changed)
	(box-pack-start h-box drawing-area)
	(connect-signal new-action  "activate" #'new)
	(connect-signal open-action "activate" #'cb-open)
	(connect-signal save-action "activate" #'save)
	(connect-signal save-as-action "activate" #'save-as)
	(connect-signal zone-settings-action "activate" #'zone-settings)
	(connect-signal disconnect-action "activate" #'disconnect-exits)
#+nil	(connect-signal window "key-press-event"
			(lambda (&rest args)
				(format *myout* "~{~a~%~}~%" args)))
	(connect-canvas-signals canvas drawing-area)
	(setf (select-cb canvas) #'room-select)
	(setf (unselect-cb canvas) #'room-unselect)
	(widget-show window)))))

(defun rounded-rectangle (x y w h r)
  (let* ((x0 (+ x r))
	 (y0 (+ y r))
	 (x1 (+ x w (- r)))
	 (y1 (+ y h (- r))))
    (arc x0 y0 r (deg-to-rad 180) (deg-to-rad 270))
    (arc x1 y0 r (deg-to-rad 270) (deg-to-rad 360))
    (arc x1 y1 r (deg-to-rad 0)   (deg-to-rad 90))
    (arc x0 y1 r (deg-to-rad 90)  (deg-to-rad 180))
    (fill-path)))

