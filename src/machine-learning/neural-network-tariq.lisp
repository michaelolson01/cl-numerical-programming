;;;; Neural network from Tariq Rashid' Book converted into lisp

;; Lisp CLOS does not handle classes the same way...
(let ((input-nodes nil)
      (hidden-nodes nil)
      (output-nodes nil)
      (activation nil)

      (learning-rate nil)
      (weights-input-hidden nil)
      (weights-hidden-output nil))

  (defun initialize-nn (nodes-input, nodes-hidden, nodes-output, rate-of-learning)
    ;; Set number of nodes for the network
    (setf input-nodes nodes-input)
    (setf hidden-nodes nodes-hidden)
    (setf output-nodes nodes-output)

    ;; Learning rate
    (setf learning-rate rate-of-learning)

    ;; activation function
    (setf activation #'sigmoid)

    ;; setup the weight matrices
    (setf weights-input-hidden (create-sample-matrix input-nodes hidden-nodes #'generate-gaussian-sample))
    (setf weights-hidden-output (create-sample-matrix hidden-nodes output-nodes #'generate-gaussian-sample)))

  ;; I pulled this out since it was the same function twice
  (defun update-weights (weights errors outputs inputs)
    "Update one set of weights once."
    (M+M weights
         (M* learning-rate
             (M• (M*M errors
                      (M*M outputs
                           (M+ 1.0 (M* -1 outputs)))) ;; 1 - outputs
                 (M.T inputs)))))

  ;; train the neural network
  (defun train-nn (inputs-list targets-list)
    (let* (;; convert input and targets to a 2d array
           (inputs (V.T inputs-list))
           (targets (V.T targets-list))

           ;; calculate signals into hidden layer
           (hidden-inputs (M• weights-input-hidden inputs))
           ;; calculate the signals emerging from the hidden layer
           (hidden-outputs (apply-activation activation hidden-inputs))

           ;; calculate signals into the final output layer
           (final-inputs (M• weights-hidden-output hidden-outputs))
           ;; calculate the signals emerging from final output layer
           (final-outputs (apply-activation activation final-inputs))

           ;; output layer error is the (target - actual)
           (output-errors (M+ targets (M* -1 final-outputs)))
           ;; hidden_layer error is the output_erros, split by weights, recombined at hidden nodes
           (hidden-errors (M• (M.T weights-hidden-output) output-errors)))

      ;; update the weights for the links between the hidden and output layers
      (setf weights-hidden-output
            (update-weights weights-hidden-output
                            output-errors
                            final-outputs
                            hidden-outputs))
      ;; update the weights for the links between the input and hidden layers
      (setf weights-input-hidden
            (update-weights weights-input-hidden
                            hidden-errors
                            hidden-outputs
                            inputs))))

  ;; query the neural network
  (defun query-nn (inputs-list)
    (let ((inputs (V.T inputs-list))
          ;; calculate the signals into hidden layer
          (hidden-inputs (M• weights-input-hidden inputs))
          ;; calculate the signals emerging from the hidden layer
          (hidden-outputs (apply-activation activation hidden-inputs))

          ;; calculate signals into final output layer
          (final-inputs (M• weights-hidden-output hidden-outputs))
          ;; calculate the signals emerging from final output layer
          (final-outputs (apply-activation activation final-inputs)))
      final-outputs)))

