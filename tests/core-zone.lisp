(in-package :cl-user)
(defpackage :lispmud/tests/core-zone
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:import-from :lispmud/core-zone #:zone #:load-zone #:save-zone #:maybe-convert-to-keyword))
(in-package :lispmud/tests/core-zone)

(5am:def-suite core-zone-suite
  :in :lispmud)

(5am:in-suite core-zone-suite)

(5am:test load-and-resave
  (let* ((zone-filename (uiop:merge-pathnames* "content/test.lzon" (asdf:system-source-directory :lispmud)))
         (zone-json (yason:parse (alexandria:read-file-into-string zone-filename)
                                 :object-key-fn #'maybe-convert-to-keyword :object-as :plist))
         (loaded-zone (load-zone zone-filename))
         (saved-zone-json (uiop:call-with-temporary-file
                           (lambda (stream filename)
                             (save-zone loaded-zone stream)
                             (with-open-file (stream1 filename)
                               (yason:parse stream1 :object-key-fn #'maybe-convert-to-keyword
                                                    :object-as :plist))))))
    (5am:is (tree-equal zone-json saved-zone-json :test #'equal))))
