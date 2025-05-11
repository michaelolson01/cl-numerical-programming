;;;; Python to lisp conversion of
;;;; [[https://www.freecodecamp.org/news/building-a-neural-network-from-scratch/][FreeCodeCamp]]

(defvar weights nil)
(defvar biases nil)

(defun init-params (layer-dims)
  (let ((L (length layer-dims)))
    (loop for l from 1 to L do
      (setf weights (cons (create-sample-matrix (nth l layer-dims) (nth (-1 l) layer-dims) #'generate-gaussian-sample)))
      (setf biases (cons (create-sample-matrix)) (create-sample-list (nth l layer-dims) (lambda () 0))))))

