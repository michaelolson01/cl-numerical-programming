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

(defun update-weight (weights errors outputs inputs learning-rate activation-prime)
  "Propagate backwards, and update the weights"
  ;; ▵Wjk = α1 * Ek * ⅆ/ⅆx α2 • Oj.T
  ;; Where
  ;; - α1 is the learning rate
  ;; - Ek is the error
  ;; - α2 is the activation function
  ;; - Oj is the outputs
  (M+M weights
       (M* learning-rate
           (M• (M*M errors (apply-activation activation-prime outputs))
               (M.T inputs)))))

(defun train-network (inputs targets weights learning-rate α α-prime)
  ;;                        ┌                                        ┐
  ;;                        │ ┌                     ┐                │
  ;; ┌             ┐        │ │ ┌    ┐   ┌        ┐ │                │
  ;; │ W11 W21 W31 │        │ │ │ E1 │   │ α'(O1) │ │                │
  ;; │             │        │ │ │    │   │        │ │   ┌          ┐ │
  ;; │ W12 W22 W32 │ + LR * │ │ │ E2 │ * │ α'(O2) │ │ • │ I1 I2 I3 │ │
  ;; │             │        │ │ │    │   │        │ │   └          ┘ │
  ;; │ W13 W23 W33 │        │ │ │ E3 │   │ α'(O3) │ │                │
  ;; └             ┘        │ │ └    ┘   └        ┘ │                │
  ;;                        │ └                     ┘                │
  ;;                        └                                        ┘
  ;; W -> W2, W1              OE -> HE     FO -> HO       HO -> I
  ;; LR * (targets - α(M• (second weights) α(M• (first weights) inputs)))
  ;; expansion –
  ;; (M* learning-rate (M• (* output-errors (apply-activation α-prime final-outputs)) (transpose (hidden-outputs))))
  ;; (M* learning-rate (M• (* (M+ targets (M* -1 final-outputs))
  ;;                          (apply-activation α-prime (apply-activation α final-inputs)))
  ;;                       (transpose (apply-activation α hidden-inputs))))
  ;; (M* learning-rate (M• (* (M+ targets (M* -1 (apply-activation α final-inputs)))
  ;;                          (apply-activation α-prime (apply-activation α (M• (second weights) hidden-outputs))))
  ;;                       (transpose (apply-activation α (M• (first weights) inputs)))))
  ;; (M* learning-rate (M• (* (M+ targets
  ;;                              (M* -1
  ;;                                  (apply-activation α
  ;;                                                    (M• (second weights)
  ;;                                                        hidden-outputs))))
  ;;                          (apply-activation α-prime
  ;;                                            (apply-activation α
  ;;                                                              (M• (second weights)
  ;;                                                                  (M• (second weights)
  ;;                                                                      hidden-inputs))))
  ;;                       (transpose (apply-activation α (M• (first weights) inputs))))))
  ;; —————————————————— final expansion of first set of weights
  ;; (M* learning-rate (M• (* (M+ targets
  ;;                              (M* -1
  ;;                                  (apply-activation α
  ;;                                                    (M• (second weights)
  ;;                                                        (apply-activation α
  ;;                                                                          hidden-inputs)))))
  ;;                          (apply-activation α-prime
  ;;                                            (apply-activation α
  ;;                                                              (M• (second weights)
  ;;                                                                  (M• (second weights)
  ;;                                                                      (M• (first weights)
  ;;                                                                          inputs)))))
  ;;                          (transpose (apply-activation α (M• (first weights) inputs))))))
  ;; —————————————————————————— Second Set
  ;; (M* learning-rate (M• (* hidden-errors (apply-activation α-prime hidden-outputs)) (transpose input)))
  ;; (M* learning-rate (M• (* (M• (M.T (second weights)) output-errors)
  ;;                          (apply-activation α-prime (apply-activation α hidden-inputs)))
  ;;                       (transpose input)))
  ;; (M* learning-rate (M• (* (M• (M.T (second weights)) (M+ targets (M* -1 final-outputs)))
  ;;                          (apply-activation α-prime (apply-activation α (M• (first weights) inputs)))
  ;;                       (transpose input)))
  ;; (M* learning-rate (M• (* (M• (M.T (second weights)) (M+ targets (M* -1 (apply-activation α final-inputs))))
  ;;                          (apply-activation α-prime (apply-activation α (M• (first weights) inputs)))
  ;;                       (transpose input))))
  ;; (M* learning-rate (M• (* (M• (M.T (second weights))
  ;;                              (M+ targets
  ;;                                  (M* -1 (apply-activation α
  ;;                                                           (M• (second weights)
  ;;                                                               (apply-activation α
  ;;                                                                                 hidden-inputs))))))
  ;;                          (apply-activation α-prime (apply-activation α (M• (first weights) inputs)))
  ;;                       (transpose input))))
  ;; ;;  —————————————————— final expansion of second set of weights
  ;; (M* learning-rate (M• (* (M• (M.T (second weights))
  ;;                              (M+ targets
  ;;                                  (M* -1 (apply-activation α
  ;;                                                           (M• (second weights)
  ;;                                                               (apply-activation α
  ;;                                                                                 (M• (first weights)
  ;;                                                                                     inputs)))))))
  ;;                          (apply-activation α-prime
  ;;                                            (apply-activation α
  ;;                                                              (M• (first weights)
  ;;                                                                  inputs))))
  ;;                       (transpose input)))

  (let* ((final-inputs (M• (second weights) hidden-outputs))
         (hidden-inputs (M• (first weights) inputs))
         (hidden-outputs (apply-activation α hidden-inputs))
         (final-outputs (apply-activation α final-inputs))
         (output-errors (M+ targets (M* -1 final-outputs)))
         (hidden-errors (M• (M.T (second weights)) output-errors))

    (setf weights-hidden-output ;; (second weights)
          (update-weight weights-hidden-output ;; weights
                         output-errors ;; errors
                         final-outputs ;; outputs
                         hidden-outputs ;; inputs
                         learning-rate ;; learning rate
                         α-prime ;; ⅆ/ⅆx of activation function
                         ))

    (setf weights-input-hidden ;; (first weights)
          (update-weight weights-input-hidden ;; weights
                          hidden-errors       ;; errors
                          hidden-outputs      ;; outputs
                          inputs              ;; inputs
                          learning-rate       ;; learning rate
                          α-prime    ;; ⅆ/ⅆx of activation-function
                          )))))

(defun train-network-rec (inputs targets weights learning-rate α α-prime &optional (accumulator nil))
  (if (not weights)
      accumulator
      (train-network-rec inputs targets (cdr weights) learning-rate α α-prime (cons (first weights) accumulator))))

;;; Neural network from Tariq Rashid' Book.
(let ((input-nodes 3)
      (hidden-nodes 3)
      (output-nodes 3)
      (activation #'sigmoid)

      (learning-rate 0.3)
      (weights-input-hidden (create-sample-matrix input-nodes hidden-nodes #'generate-gaussian-sample))
      (weights-hidden-output (create-sample-matrix hidden-nodes output-nodes #'generate-gaussian-sample)))

  (defun query-nn (inputs)
    ;; α(wh-o • α (wi-h • inputs) = output
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
