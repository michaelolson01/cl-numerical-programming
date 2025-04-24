(in-package cl-numerical-programming)

(defparameter π pi)

(defun random-exclusive-0 (upper-limit)
  "This is like a (random upper-limit) but is exclusive for 0.0."
  ;; optimize for single-float
  (declare (type single-float upper-limit))
  ;; make sure user entered a single-float
  (check-type upper-limit single-float)
  ;; get a random number 0.0 < result < upper-limit
  (loop for r = (random upper-limit)
        until (> r 0.0)
        finally (return r)))

;; Gaussian or Normal Distribution Sample
(defun generate-gaussian-sample (&optional (μ 0.0) (σ 1.0))
  "Gets a sample of a gaussian distribution by the Box-Mueller method
default μ is 0.0 default σ is 1.0 (Standard Normal Distibution)"
  ;; optimize for single-float
  (declare (type single-float μ)
           (type single-float σ))
  ;; make sure user entered a single-float
  (check-type μ single-float)
  (check-type σ single-float)
  (let* ((u1 (random-exclusive-0 1.0))
         (u2 (random 1.0))
         (θ (* pi 2 u2))
         (magnitude (* σ (sqrt (* -2.0 (log u1)))))
         (z0 (+ (* magnitude (cos θ)) μ)))
    ;; We only need one of these, and I can't find if
    ;; lisp has implemented a function to get both at the same time (soft FSINCOS)
    ;; (z1 (+ (* magnitude (sin θ)) μ1)))
    z0))
