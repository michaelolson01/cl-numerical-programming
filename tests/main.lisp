(defpackage cl-numerical-programming/tests/main
  (:use :cl
        :cl-numerical-programming
        :rove))
(in-package :cl-numerical-programming/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-numerical-programming)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
