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
;;; Will be adding later
(defun initialize-biases (layers &optional accumulator)
  (if (= 1 (length layers))
      (reverse accumulator)
      (initialize-biases (cdr layers)
                         (cons (create-sample-list (second layers)
                                                   (lambda () 0.0))
                               accumulator))))

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

  ;; The functions to evaluate the errors is different, is it a simplificcation?
  ;;                           ┌              ┐
  ;;                  ┌    ┐   │      ┌     ┐ │
  ;;                  │ T1 │   │      │ FO1 │ │
  ;;                  │    │   │      │     │ │
  ;;  output-errors = │ T2 │ + │ -1 * │ FO2 │ │
  ;;                  │    │   │      │     │ │
  ;;                  │ T3 │   │      │ FO3 │ │
  ;;                  └    ┘   │      └     ┘ │
  ;;                           └              ┘

  ;;                  ┌             ┐
  ;;                  │ T1 - α(FI1) │
  ;;                  │             │
  ;;  output-errors = │ T2 - α(FI2) │
  ;;                  │             │
  ;;                  │ T3 - α(FI3) │
  ;;                  └             ┘
  ;;
  ;; For output-erros, this is from the output, and the outputs have no weights assigned to them, so
  ;;                  ┌       ┐   ┌             ┐
  ;;                  │ 1 1 1 │   │ T1 - α(FI1) │
  ;;                  │       │   │             │
  ;;  output-errors = │ 1 1 1 │ • │ T2 - α(FI2) │
  ;;                  │       │   │             │
  ;;                  │ 1 1 1 │   │ T3 - α(FI3) │
  ;;                  └       ┘   └             ┘
  ;;
  ;;                  ┌             ┐   ┌             ┐
  ;;                  │ W11 W12 W13 │   │ T1 - α(FI1) │
  ;;                  │             │   │             │
  ;;  hidden-errors = │ W21 W22 W23 │ • │ T2 - α(FI2) │
  ;;                  │             │   │             │
  ;;                  │ W31 W32 W33 │   │ T3 - α(FI3) │
  ;;                  └             ┘   └             ┘


  (let* ((hidden-inputs (M• (first weights) inputs))
         (hidden-outputs (apply-activation α hidden-inputs))
         (final-inputs (M• (second weights) hidden-outputs))
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

;; write this
(defun train-network-rec (inputs targets weights learning-rate α α-prime &optional (accumulator nil))
  (if (not weights)
      accumulator
      (train-network-rec inputs targets (cdr weights) learning-rate α α-prime (cons (first weights) accumulator))))
