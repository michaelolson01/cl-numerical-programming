;;;; Python to lisp conversion of
;;;; [[https://www.freecodecamp.org/news/building-a-neural-network-from-scratch/][FreeCodeCamp]]

;;; Initialize weights and biases
(defun init-params (layer-dims)
  (let ((weights nil)
        (biases nil)
        (L (length layer-dims)))
    (loop for l from 1 to L do
      (setf weights (cons (create-sample-matrix (nth l layer-dims)
                                                (nth (-1 l) layer-dims)
                                                #'generate-gaussian-sample)
                          weights))
      (setf biases (cons (create-sample-list (nth l layer-dims)
                                             (lambda () 0))
                         biases))
      (values weights biases))))

;;; Sigmoid is already defined, but this one is adding to a cache.
(defun sigmoid-ffc (Z)
  (values (apply-activation #'sigmoid Z) Z))

;;; This has already been re-written, without the biases and cache.
(defun forward-propagation-ffc (X weights biases)
  (let ((A X)
        (caches nil)
        (L (length weights)))
    (loop for l from 1 to (+1 L) do
      (let ((A-prev A)
            (Z (M+ (M• (nth l weights) A-prev) (nth l biases)))
            (linear-cache (list A-prev (nth l weights) (nth l biases)))
            ((muliple-value-bind (A activation-cache) (sigmoid-ffc Z)))
            (cache (list linear-cache activation-cache)))
        (setf caches (cons cache caches))))
    (values A caches)))

;;; Wow that looks messy.
(defun cost-function (A Y)
  (let ((m (length (first Y))))
    (M* (/ -1 m) (M+M (M• (apply-activation #'log A) (M.T Y))
                      (M• (apply-activation #'log (M+ 1 (M* -1 A)))
                          (M+ 1 (M* -1 (M.T Y))))))))

