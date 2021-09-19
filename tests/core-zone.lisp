(in-package :cl-user)
(defpackage :lispmud/tests/core-zone
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:import-from :lispmud/core-zone #:zone #:load-zone #:save-zone))
(in-package :lispmud/tests/core-zone)

(5am:def-suite core-zone-suite
  :in :lispmud)

(5am:in-suite core-zone-suite)

(5am:test load-and-resave
  (let* ((zone-filename (uiop:merge-pathnames* "content/test.lzon" (asdf:system-source-directory :lispmud)))
         (zone-sexp (with-open-file (stream zone-filename)
                      (let ((*package* (find-package :lispmud)))
                        (read stream))))
         (loaded-zone (load-zone zone-filename))
         (saved-zone-sexp (uiop:call-with-temporary-file
                           (lambda (stream filename)
                             (declare (ignore stream))
                             (save-zone loaded-zone filename)
                             (with-open-file (stream zone-filename)
                               (let ((*package* (find-package :lispmud)))
                                 (read stream)))))))
    (5am:is (equal zone-sexp saved-zone-sexp))))
