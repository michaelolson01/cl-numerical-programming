(in-package :cl-numerical-programming)

;; To initialize the weights to random numbers.
(defun initialize-weights (layers &optional accumulator)
  (if (= 1 (length layers))
      (reverse accumulator)
      (initialize-weights (cdr layers)
                          (cons (create-sample-matrix (second layers)
                                                      (first layers)
                                                      #'generate-gaussian-sample)
                                accumulator))))

;; Have not gotten this far in the book yet.
(defun initialize-biases (layers &optional accumulator)
  (if (= 1 (length layers))
      (reverse accumulator)
      (initialize-weights-rec (cdr layers)
                              (cons (create-sample-list (second layers)
                                                        (lambda () 0.0))
                                    accumulator))))

;; helper functions
;; only because it is faster to do complex calculations once, and then multiply the answers together.
(defun ^2 (x)
  (* x x))

(defun ^3 (x)
  (* x x x))

;; activation functions
(defun unit-step (x)
  (cond ((< x 0) 0)
        ((= x 0) 0.5)
        (t 1))) ;; (> x 0)

(defun signum (x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        (t 1))) ;; (> x 0)

(defun linear (x)
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
  (mapcar (lambda (input) (mapcar function input)) matrix))

;; for a two layer fully connected network
;; (hw x) = (g2 (dot W2 (g1 (dot W1 x))))
;; whereas g1 and g2 are the activation functions for the first and second layers.
;; W1 and W2 are the weight matrices of the first and second layers
;; x is the inputs layer.
;;
;; activation functions never seem to change, so it is simplified to use one function.
;; so, recursively -
(defun forward-propagation (inputs activation-function weights &optional (caches nil))
  (if (not weights) ;; if we are done
      (values inputs caches)
      (let ((linear-hypothesis (matrix-vector-multiplication (first weights)
                                                             inputs)))
        (forward-propogation (mapcar activation-function linear-hypothesis)
                             activation-function
                             (cdr weights)
                             (cons (list (list inputs (first weights)) linear-hypothesis) caches)))))

;; Neural network from Tariq Rashid' Book.
(let ((input-nodes 3)
      (hidden-nodes 3)
      (output-nodes 3)
      (activation-function #'sigmoid)

      (learning-rate 0.3)
      (weights-input-hidden (create-sample-matrix 3 3 #'generate-gaussian-sample))
      (weights-hidden-output (create-sample-matrix 3 3 #'generate-gaussian-sample)))

  (defun query-nn (inputs)
    (apply-activation-function activation-function
                               (matrix-dot weights-hidden-output
                                           (apply-activation-function activation-function
                                                                      (matrix-dot weights-input-hidden
                                                                                  inputs)))))

  (defun update-weights (weights errors outputs inputs)
    (matrix-combine #'+
                    weights
                    (matrix-apply (lambda (input) (* learning-rate input))
                                  (matrix-dot (matrix-combine #'*
                                                              (matrix-combine #'*
                                                                              errors
                                                                              outputs)
                                                              (matrix-apply (lambda (input) (+ 1 input))
                                                                            (matrix-apply (lambda (input) (* -1 input))
                                                                                          outputs)))
                                              (transpose-matrix-list inputs)))))

  (defun train-nn (inputs-list targets-list)
    (let* ((inputs (transpose-matrix-list (list inputs-list))) ;;
           (targets (transpose-matrix-list (list targets-list)))
           (hidden-inputs (matrix-dot weights-input-hidden inputs))
           (hidden-outputs (apply-activation-function activation-function hidden-inputs)) ;;
           (final-inputs (matrix-dot weights-hidden-output hidden-outputs))
           (final-outputs (apply-activation-function activation-function final-inputs)) ;;
           (output-errors (matrix-apply #'+ targets (scalar-matrix-multiplication -1 final-outputs))) ;;
           (hidden-errors (matrix-dot (transpose-matrix-list weights-hidden-output) output-errors))) ;;
      (setf weights-hidden-output (update-weights weights-hidden-output output-errors final-outputs hidden-outputs))
      (setf weights-input-hidden (update-weights weights-input-hidden hidden-errors hidden-outputs inputs)))))
