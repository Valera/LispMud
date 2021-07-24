(in-package :cl-user)
(defpackage :lispmud/tests/text-editor
  (:use :cl)
  (:use :lispmud/core-threadvars)
  (:import-from :lispmud/core-zone #:zone)
  (:import-from :lispmud/core-server #:client)
  (:import-from :lispmud/text-editor #:text-editor-fsm #:process-input1))
(in-package :lispmud/tests/text-editor)

(5am:def-suite text-editor-fsm-suite
  :in :lispmud)

(5am:in-suite text-editor-fsm-suite)

(defun make-mocked-zone ()
  (list (make-instance 'zone :name "Mocked zone" :entry-rooms (list :mocked-room))))

(5am:def-test text-editor-fsm ()
  (let* (result-text
         (editor (make-instance 'text-editor-fsm
                                :after-editing-cb (lambda (text) (setf result-text text))))
         (*zone-list* (make-mocked-zone))
         (*client* (make-instance 'client :input-handlers (list editor)
                                          :out-stream (make-string-output-stream))))
    (process-input1 editor (format nil "Привет~%"))
    (process-input1 editor (format nil "Мир!~%"))
    (process-input1 editor (format nil "\\s~%"))
    (5am:is (equal result-text (format nil "Привет~%Мир!~%")))))
