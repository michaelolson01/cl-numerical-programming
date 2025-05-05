(in-package :cl-numerical-programming)

;;; helper functions
;;; only because it is faster to do complex calculations once, and then multiply the answers together.
(defun ^2 (x)
  (* x x))

(defun ^3 (x)
  (* x x x))
