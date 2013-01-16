(in-package :mudsketcher)

(defun add-buttons-to-dialog (dialog)
  "Add accept of cancel button to dialog"
  (dialog-add-button dialog "gtk-cancel" :cancel)
  (dialog-add-button dialog "gtk-new" :accept))

(defun run-special-mob-dialog (builder mob-plist)
  (assert (getf mob-plist :type))
  (bind:bind
      (((dialog-name fields-plist) (getf *service-mob-properties*
                                         (getf mob-plist :type)))
       (dialog (builder-get-object builder dialog-name)))
    (widget-show dialog)
    (iter (for (field-key widget-name) on fields-plist by #'cddr)
          (for property-widget = (builder-get-object builder widget-name))
          (assert property-widget nil "Can not find widget with name ~A" widget-name)
	  (setf (text-buffer-text (text-view-buffer property-widget))
		(or (getf mob-plist field-key) "")))
    (let ((dialog-result (dialog-run dialog)))
      (widget-hide dialog)
      (when (eq :accept dialog-result)
	(iter (for (field-key widget-name) on fields-plist by #'cddr)
	      (setf (getf mob-plist field-key)
		    (text-buffer-text (text-view-buffer
				       (builder-get-object builder widget-name))))))
      mob-plist)))

(defun add-plist-to-plist (plist1 plist2)
  (iter (for (key val) on plist2 by #'cddr)
	(setf (getf plist1 key) val))
  plist1)

#+nil
(defun run-banker-dialog (builder mob-plist)
  (run-special-mob-dialog builder "banker-mob-options-dialog" mob-plist
			  '(:deposit-phrase (list "deposit-phrase-textview" 
			    :withdraw-phrase "withdraw-phrase-textview"))))

(defclass mob-editor ()
  ((mob-hash :reader mob-hash)
   (mob-list)
   (builder)))

(defmethod mobs-spec ((mob-editor mob-editor))
  (flush-current-mob mob-editor)
  (hash-table-values (mob-hash mob-editor)))

(defmethod (setf mobs-spec) (new-spec (mob-editor mob-editor))
  (with-slots (mob-hash mob-list) mob-editor
    (iter (for i from (1- (store-items-count mob-list)) downto 0)
          (store-remove-item mob-list (aref (gtk::store-items mob-list) i)
                             :test #'string=))
    (iter (for mob-spec in new-spec)
          (for name = (getf mob-spec :name))
          (store-add-item mob-list name)
          (setf (gethash name mob-hash) mob-spec))))

(defgeneric flush-current-mob (mob-editor)
  (:documentation "EQL-specialized in INIT-INSTANCE"))

(defgeneric show-mob-select-dialog (mob-editor))
(defmethod show-mob-select-dialog ((mob-editor mob-editor))
  (with-slots (builder mob-list) mob-editor
    (let ((dialog (builder-get-object builder "mob-select-dialog"))
          ;(mob-list (make-instance 'array-list-store))
          (mob-list-view (builder-get-object builder "mob-select-dialog.list-view")))
;      (store-add-column mob-list "gchararray" #'identity)
      (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
        (cell-layout-pack-start mob-list-view renderer :expand t)
        (cell-layout-add-attribute mob-list-view renderer "text" 0))
      (setf (tree-view-model mob-list-view) mob-list)
      (add-buttons-to-dialog dialog)
      (widget-show dialog)
      (let ((dialog-result (dialog-run dialog)))
        (widget-hide dialog)
        (if (eq :accept dialog-result)
            (tree-model-item (tree-view-model mob-list-view)
                             (tree-view-get-cursor mob-list-view))
            nil)))))

(defparameter *service-mob-properties*
  '(:banker
    ("banker-mob-options-dialog"
     (:deposit-phrase "deposit-phrase-textview"
      :withdraw-phrase "withdraw-phrase-textview"))
    :seller
    ("seller-options"
     (:buy-phrase "seller-options.buy-phrase-entry"
      :buy-see-others "seller-options.buy-see-others-entry"
      :sell-phrase "seller-options.sell-phrase-entry"
      :sell-see-others "seller-options.sell-see-others-entry"))))


(defmethod initialize-instance :after ((mob-editor mob-editor) &key gtk-builder)
  (flet ((bgo (object-id) (builder-get-object gtk-builder object-id)))
    (with-slots (mob-hash mob-list builder) mob-editor
      (setf mob-hash (make-hash-table :test 'equal)
            mob-list (make-instance 'array-list-store)
            builder gtk-builder)
      (let ((current-mob nil)
            (f-change-mob-name nil)
            (mob-list-view (bgo "treeview2"))
            (mob-editor-name (bgo "mob-name-entry"))
            (mob-editor-enter-verb (bgo "mob-enter-verb-entry"))
            (mob-editor-leave-verb (bgo "mob-leave-verb-entry"))
            (mob-editor-animation-1 (bgo "mob-animation-1-entry"))
            (mob-editor-animation-2 (bgo "mob-animation-2-entry"))
            (mob-editor-animation-1-timer (bgo "mob-animation-1-timer"))
            (mob-editor-animation-2-timer (bgo "mob-animation-2-timer"))
            (mob-editor-move-interval (bgo "move-interval-spinbutton"))
            (mob-editor-type-combo-box (bgo "combobox2"))
            (mob-editor-type-names '("Обычный" "Банкир" "Продавец" "Кладовщик"))
            (mob-editor-types '(:regular :banker :seller :storekeeper))
            (mob-editor-type-model (make-instance 'array-list-store))
            (mob-editor-optons-button (bgo "button1"))
            (add-mob-action (bgo "add-mob"))
            (remove-mob-action (bgo "remove-mob"))
            (mob-number-factory 0))
        (labels
            ((debug-out (str)
               (declare (ignorable str))
               #+(or)
               (setf (text-buffer-text (text-view-buffer text-view))
                     (format nil "~s" str)))
             (initialize-mob-list (m c)
               (store-add-column m "gchararray" #'identity)
               (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
                 (cell-layout-pack-start c renderer :expand t)
                 (cell-layout-add-attribute c renderer "text" 0)))
             (add-mob (&rest args)
               (declare (ignore args))
               (setf f-change-mob-name nil)
               (let ((new-mob-id (format nil "new mob ~a" (incf mob-number-factory))))
                 (store-add-item mob-list new-mob-id)
                 (tree-view-set-cursor mob-list-view
                                       (tree-model-path mob-list (tree-model-iter-first mob-list)))
                 (setf current-mob new-mob-id)
                 (setf (gethash new-mob-id mob-hash)
                       (list :name new-mob-id :type :regular))
                 (populate-mob-options new-mob-id)
                 (tree-view-set-cursor mob-list-view (tree-model-path mob-list (tree-model-iter-from-string mob-list (format nil "~A" (1- (store-items-count mob-list)))))))
               (setf f-change-mob-name t))
             (remove-mob (&rest args)
               (declare (ignore args))
               (when current-mob
                 (remhash current-mob mob-hash)
                 (store-remove-item mob-list current-mob :test #'string=)))
             (mob-name-changed (entry)
               (when f-change-mob-name
                 (let ((mob-plist (gethash current-mob mob-hash)))
                   (remhash current-mob mob-hash)
                   (setf current-mob (entry-text entry)
                         (getf mob-plist :name) current-mob
                         (gethash current-mob mob-hash) mob-plist))
                 ;; ACTUNG!!! FIXME!!!
                 (setf
                  (aref (gtk::store-items mob-list)
                        (parse-integer (tree-model-iter-to-string
                                        mob-list
                                        (tree-model-iter-by-path
                                         mob-list
                                         (tree-view-get-cursor mob-list-view)))))
                  (entry-text entry))
                 (widget-queue-draw mob-list-view)))
             (change-edited-mob (tree-view)
               (debug-out (hash-table-count mob-hash))
               (when current-mob
                 (save-mob-options current-mob))
               (setf current-mob (tree-model-item (tree-view-model tree-view)
                                                  (tree-view-get-cursor tree-view)))
               (populate-mob-options current-mob))
             (save-mob-options (mob-name)
               (setf (gethash mob-name mob-hash)
                     (add-plist-to-plist
                      (gethash mob-name mob-hash)
                      (list :name (entry-text mob-editor-name)
                            :enter-verb (entry-text mob-editor-enter-verb)
                            :leave-verb (entry-text mob-editor-leave-verb)
                            :move-interval (spin-button-value mob-editor-move-interval)
                            :animation-1 (entry-text mob-editor-animation-1)
                            :animation-2 (entry-text mob-editor-animation-2)
                            :animation-1-timer
                            (spin-button-value mob-editor-animation-1-timer)
                            :animation-2-timer
                            (spin-button-value mob-editor-animation-2-timer)))))
             (populate-mob-options (mob-id)
               (destructuring-bind
                     (&key name enter-verb leave-verb move-interval animation-1 animation-2
                           animation-1-timer animation-2-timer type &allow-other-keys)
                   (gethash mob-id mob-hash)
                 (setf (entry-text mob-editor-name) (or name "")
                       (entry-text mob-editor-enter-verb) (or enter-verb "")
                       (entry-text mob-editor-leave-verb) (or leave-verb "")
                       (spin-button-value mob-editor-move-interval) (or move-interval 0)
                       (entry-text mob-editor-animation-1) (or animation-1 "")
                       (entry-text mob-editor-animation-2) (or animation-2 "")
                       (spin-button-value mob-editor-animation-1-timer) (or animation-1-timer 0)
                       (spin-button-value mob-editor-animation-2-timer) (or animation-2-timer 0)
                       (combo-box-active mob-editor-type-combo-box) (or (position type mob-editor-types) 0))))
             (mob-editor-type-changed (combo)
               (setf (getf (gethash current-mob mob-hash) :type)
                     (nth (combo-box-active combo) mob-editor-types))
               (setf (widget-sensitive mob-editor-optons-button)
                     (/= 0 (combo-box-active combo)))))
          (connect-signal mob-editor-type-combo-box "changed" #'mob-editor-type-changed)
          (initialize-model-and-combo-box
           mob-editor-type-model mob-editor-type-combo-box mob-editor-type-names)
          (connect-signal add-mob-action "activate" #'add-mob)
          (connect-signal remove-mob-action "activate" #'remove-mob)
          (initialize-mob-list mob-list mob-list-view)
          (setf (tree-view-model mob-list-view) mob-list)
          (connect-signal mob-list-view "cursor-changed" #'change-edited-mob)
          (connect-signal mob-editor-name "changed" #'mob-name-changed)
          (iter (for (dialog-name fields-spec) in (rest *service-mob-properties*)
                     by #'cddr)
                (add-buttons-to-dialog (bgo dialog-name)))
          (connect-signal
           mob-editor-optons-button "clicked"
           #'(lambda (b) (declare (ignore b))
                     (setf (gethash current-mob mob-hash)
                           (run-special-mob-dialog
                            builder (gethash current-mob mob-hash)))))
          (defmethod flush-current-mob ((mob-editor (eql mob-editor)))
            (save-mob-options current-mob)))))))
