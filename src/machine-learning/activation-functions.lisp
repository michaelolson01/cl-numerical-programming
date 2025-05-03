(in-package :cl-numerical-programming)

;;; activation functions

(defun unit-step (x)
  "Used in the perceptatron"
  (cond ((< x 0) 0)
        ((= x 0) 0.5)
        (t 1))) ;; (> x 0)

(defun signum (x)
  "unit-step centered around 0"
  (cond ((< x 0) -1)
        ((= x 0) 0)
        (t 1))) ;; (> x 0)

(defun linear (x)
  "Yes, it's linear. (= x x)."
  x)

(defun piece-wise-linear (x)
  (cond ((>= x 0.5) 1)
        ((or (< x -0.5) (> x 0.5)) (+ x 0.5))
        (t 0))) ;; (<= x 0.5)

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun sigmoid-prime (x)
  (* (sigmoid x) (- 1 (sigmoid x))))

(defun shifted-relu (x &optional (shift -1))
  (max shift x))

(defun relu (x)
  (max 0 x))

(defun relu-prime (x)
  (if (> x 0)
      1
      0))

(defun leaky-relu (x &optional (α 0.01))
  (if (>= x 0)
      x
      (* α x)))

(defun leaky-relu-prime (x &optional (α 0.01))
  (if (> x 0)
      1
      α))

(defun softplus (x)
  (log (+ 1 (exp x))))

(defun softplus-prime (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun elu (x &optional (α 1.0))
  (if (> x 0)
      x
      (* α (- (exp x) 1))))

(defun elu-prime (x &optional (α 1.0))
  (if (> x 0)
      1
      (* α (exp x))))

(defun selu (x &optional (α 1.0) (scale 1.0))
  (if (> x 0)
      (* s x)
      (* s α (- (exp x) 1))))

(defun selu-prime (x &optional (α 1.0) (scale 1.0))
  (if (> x 0)
      s
      (* scale α (exp x))))


;; from wolfram
;; found on cppreference.com
(defun tanh-original (x)
  "This is the true mathematical equation taken from Wolfram"
  (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))

;; from AI a Modern Approach
(defun tanh-2 (x)
  "This is taken from AI a Modern Approach, and seems to be the standard used in machine learning."
  (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)))

;; from Geeks for Geeks
(defun tanh-3 (x)
  "This is taken from the Geeks for Geeks website."
  (- (/ 2 (+ 1 (exp (* -2 x)))) 1))

(defun tanh-prime (x)
  (- 1 (^2 (tanh x))))

(defun apply-activation-function (function matrix)
  "Applies the activation function to every element of a matrix."
  ;; This should use an apply function from matrices.
  (mapcar (lambda (input) (mapcar function input)) matrix))
