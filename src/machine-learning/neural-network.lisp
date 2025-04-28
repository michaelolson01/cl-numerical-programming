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

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun relu (x)
  (max 0 x))

(defun softplus (x)
  (log (+ 1 (exp x))))

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

(defun apply-activation-function (function vector &optional accumulator)
  "Apply the activation function to a vector"
  (if (null vector)
      (reverse accumulator)
      (apply-activation-function function
                                 (cdr vector)
                                 (cons (funcall function (first vector)) accumulator))))

(defun simple-feedforward-network (layers-input)
  "runs a simple feedforward network.
argument layers is a list of layers, and the nodes in the layer
for instance (3 3 2) has 3 nodes in the first (input), 3 in the first hidden and
2 in the second hidden. the output is always 1 node, and does not need to be added."
  (let* ((layers (append layers-input '(1)))
         (weights (initialize-weights layers)))
    weights))

;; for a two layer fully connected network
;; (hw x) = (g2 (dot W2 (g1 (dot W1 x))))
;; whereas g1 and g2 are the activation functions for the first and second layers.
;; W1 and W2 are the weight matrices of the first and second layers
;; x is the inputs layer.
;;
;; activation functions never seem to change, so it is simplified to use one function.
;; so, recursively -
(defun forward-propogation (inputs activation-function weights &optional (accumulator null) (caches null))
  (if (null weights) ;; if we are done
      accumulator
      (if (null accumulator) ;; we haven't started yet
          (hw inputs
              activation-function
              (cdr weights)
              (apply-activation-function activation-function (dot (first weights) inputs)))
          (hw inputs
              activation-function
              (cdr weights)
              (apply-activation-function activation-function (dot (first weights) accumulator))))))
;; now, obviously, this cannot change, there is no way to train this model at the moment.

