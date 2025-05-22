(defpackage cl-numerical-programming/tests/main
  (:use :cl
        :cl-numerical-programming
        :rove))
(in-package :cl-numerical-programming/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-numerical-programming)' in your Lisp.

(deftest test-target-1
    (testing "should (= 1 1) to be true"
	   (ok (= 1 1))))

(deftest matrix-reshape-test-1
    (testing "matrix reshape test 1"
	     (let ((matrix '((1 2 3) (4 5 6))))
	       (ok (= (matrix-reshape matrix 2 3) '((1 2) (3 4) (5 6)))))))
1
