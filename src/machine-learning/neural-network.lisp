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
  (/ 1 (+ 1 (exp (- 1 x)))))

(defun relu (x)
  (max 0 x))

(defun softplus (x)
  (log (+ 1 (exp x))))

;; These seem to give similar answer as the built-in (tanh)
;; I question as to how accurate these need to be...
;; from wolfram
(defun tanh-original (x)
  (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))

;; from AI a modern Approach
(defun tanh-2 (x)
  (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)))

(defun get-edges-between-layers (first-layer second-layer &optional accumulator)
  )

;; I always have to take a minute to think about how to organize these.
;; This will have to create the edges between the nodes
(defun print-computation-graph (layers-input &optional accumulator)
  "x is the inputs, g is the nodes of other layers (including the output), w is weight"
  (if (= 1 (length layers-input))
      ;; we are on the input layer
      accumulator
      (let ((edges ()))
        ())))

(defun simple-feedforward-network (layers-input)
  "runs a simple feedforward network.
argument layers is a list of layers, and the nodes in the layer
for instance (3 3 2) has 3 nodes in the first (input), 3 in the first hidden and
2 in the second hidden. the output is always 1 node, and does not need to be added."
  (let* ((layers (append layers-input '(1)))
         (weights (initialize-weights layers)))
    weights))
