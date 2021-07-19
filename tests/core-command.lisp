(in-package :cl-user)
(defpackage :lispmud/tests/core-command
  (:use :cl)
  (:import-from :alexandria
                #:hash-table-keys #:set-equal)
  (:import-from :lispmud/core-command
                #:init-command-table #:*command-hash* #:exec-command #:short-match-p
                #:word-dispatch))
(in-package :lispmud/tests/core-command)

(5am:def-suite :lispmud)

(5am:def-suite command
  :in :lispmud
  :description "command test suite")

(5am:in-suite command)

(5am:def-fixture setup-command-hash (commands-spec)
  (let ((*command-hash* (make-hash-table :test 'equal)))
    (init-command-table commands-spec)
    (&body)))

(5am:test init-command-table
  (5am:with-fixture setup-command-hash ('(("abcd" #'id)))
    (5am:is (set-equal (hash-table-keys *command-hash*)
                       '("a" "ab" "abc" "abcd")
                       :test #'string=))))

(5am:test exec-command
  (let ((last-command))
    (labels
        ((f (&rest data) (setf last-command `(f ,data)))
         (g (&rest data) (setf last-command `(g ,data)))
         (run-test (command)
           (setf last-command nil)
           (exec-command command)
           last-command))
      (5am:with-fixture setup-command-hash (`(("abc" ,#'f) ("abD" ,#'g)))
        (5am:is (equal (run-test "abc a b c") '(f ("a" "b" "c"))))
        (5am:is (equal (run-test "ab a b c") '(f ("a" "b" "c"))))
        (5am:is (equal (run-test "abD a") '(g ("a"))))
        (5am:is (eq (run-test "hello world") nil))
        (5am:is (eq (run-test "") nil))))))

(5am:test short-match-p
  (5am:is (short-match-p "abc" "abc"))
  (5am:is (short-match-p "abc" "ab"))
  (5am:is (short-match-p "abc" "a"))
  (5am:is (short-match-p "abc" ""))
  (5am:is (not (short-match-p "abc" "abD"))))

(5am:test word-dispatch
  (let ((x))
    (labels
        ((test-word (word)
           (setf x nil)
           (word-dispatch word
             ("abc" (setf x "abc"))
             ("abD" (setf x "abD"))
             (t (setf x "default")))
           x))
      (5am:is (equal "abc" (test-word "abc")))
      (5am:is (equal "abc" (test-word "ab")))
      (5am:is (equal "abD" (test-word "abD")))
      (5am:is (equal nil (test-word "")))
      (5am:is (equal "default" (test-word "xyz"))))))
