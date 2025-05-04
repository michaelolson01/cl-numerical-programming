;;;; Implement a neural network in lisp
;;;; This uses lists, so could be optimized with vectors in the future.

(in-package :cl-numerical-programming)

;;; To initialize the weights to random numbers.
(defun initialize-weights (layers &optional accumulator)
  (if (= 1 (length layers))
      (reverse accumulator)
      (initialize-weights (cdr layers)
                          (cons (create-sample-matrix (second layers)
                                                      (first layers)
                                                      #'generate-gaussian-sample)
                                accumulator))))

;;; Have not learned about biases yet
(defun initialize-biases (layers &optional accumulator)
  (if (= 1 (length layers))
      (reverse accumulator)
      (initialize-biases (cdr layers)
                         (cons (create-sample-list (second layers)
                                                   (lambda () 0.0))
                               accumulator))))

;;; helper functions
;;; only because it is faster to do complex calculations once, and then multiply the answers together.
(defun ^2 (x)
  (* x x))

(defun ^3 (x)
  (* x x x))

(defun forward-propagation (inputs activation weights)
  "Propagate forward in the neural network"
  ;; Interestingly enough, the number of nodes between layers is inferred by the
  ;; current matrix of weights.
  ;; inputs | next layer
  ;;      1 |          5
  ;;      2 |          6
  ;;      3 |          7
  ;; weights will be 3 for each node on the next layer, so no need to pass the next layer.
  ;; input updates at end of the processing.
  (if (not weights)
      ;; No weights means we are done processing this pass.
      inputs
      (let ((linear-hypothesis (M• (first weights)
                                   inputs)))
        (forward-propagation (apply-activation activation linear-hypothesis)
                             activation
                             (cdr weights)))))

(defun update-weight (weights errors outputs inputs learning-rate activation-sigma)
  "Propagate backwards, and update the weights"
  ;; ▵Wjk = α1 * Ek * ⅆ/ⅆx α2 • Oj.T
  ;; Where
  ;; - α1 is the learning rate
  ;; - Ek is the error
  ;; - α2 is the activation function
  ;; - Oj is the outputs
  (M+M weights
       (M* learning-rate
           (M• (M*M errors (apply-activation activation-sigma outputs))
               (M.T inputs)))))

;; Neural network from Tariq Rashid' Book.
(let (;+(input-nodes 3) ;; Never actually used in the calculations
      ;+(hidden-nodes 3)
      ;+(output-nodes 3)
      (activation #'sigmoid)

      (learning-rate 0.3)
      (weights-input-hidden (create-sample-matrix 3 3 #'generate-gaussian-sample))
      (weights-hidden-output (create-sample-matrix 3 3 #'generate-gaussian-sample)))

  (defun query-nn (inputs)
    ;; α(wh-o dot α (wi-h dot inputs) = output
    (apply-activation activation
                      (M• weights-hidden-output
                          (apply-activation activation
                                            (M• weights-input-hidden
                                                inputs)))))

  (defun update-weights (weights errors outputs inputs)
    "Update one set of weights once."
    (M+M weights
         (M* learning-rate
             (M• (M*M errors
                      (M*M outputs
                           (M+ 1.0 (M* -1 outputs)))) ;; 1 - outputs
                 (M.T inputs)))))

  (defun train-nn (inputs-list targets-list)
    (let* ((inputs (V.T list inputs-list)) ;; It expects the inputs to be (1 2 3) not ((1) (2) (3))
           (hidden-inputs (M• weights-input-hidden inputs))
           (hidden-outputs (apply-activation activation hidden-inputs))
           (final-inputs (M• weights-hidden-output hidden-outputs))
           (final-outputs (apply-activation activation final-inputs))
           (targets (V.T targets-list))
           (output-errors (M+ targets (M* -1 final-outputs)))
           (hidden-errors (M• (M.T weights-hidden-output) output-errors)))
      (setf weights-hidden-output
            (update-weights weights-hidden-output
                            output-errors
                            final-outputs
                            hidden-outputs))
      (setf weights-input-hidden
            (update-weights weights-input-hidden
                            hidden-errors
                            hidden-outputs
                            inputs)))))
