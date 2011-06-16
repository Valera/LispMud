(in-package :mudsketcher)

(defun add-buttons-to-dialog (dialog)
  "Add accept of cancel button to dialog"
  (dialog-add-button dialog "gtk-cancel" :cancel)
  (dialog-add-button dialog "gtk-new" :accept))

(defun run-special-mob-dialog (builder dialog-name mob-plist fields-plist)
  (let ((dialog (builder-get-object builder dialog-name)))
    (widget-show dialog)
    (iter (for (field-key widget-name) on fields-plist by #'cddr)
	  (setf (text-buffer-text (text-view-buffer (builder-get-object builder widget-name)))
		(or (getf mob-plist field-key) "")))
    (let ((dialog-result (dialog-run dialog)))
      (widget-hide dialog)
      (when (eq :accept dialog-result)
	(iter (for (field-key widget-name) on fields-plist by #'cddr)
	      (setf (getf mob-plist field-key)
		    (text-buffer-text (text-view-buffer
				       (builder-get-object builder widget-name)))))
	mob-plist))))

(defun add-plist-to-plist (plist1 plist2)
  (iter (for (key val) on plist2 by #'cddr)
	(setf (getf plist1 key) val))
  plist1)

#+nil
(defun run-banker-dialog (builder mob-plist)
  (run-special-mob-dialog builder "banker-mob-options-dialog" mob-plist
			  '(:deposit-phrase (list "deposit-phrase-textview" 
			    :withdraw-phrase "withdraw-phrase-textview"))))