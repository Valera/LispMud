(in-package :cl-user)
(defpackage :lispmud/core-globalvars
  (:use :cl)
  (:export #:*alpha-version-password* #:*db-connection-spec*))
(in-package :lispmud/core-globalvars)

(defvar *alpha-version-password*)
(defvar *db-connection-spec*)
