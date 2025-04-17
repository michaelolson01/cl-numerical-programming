(in-package cl-numeric-programming)

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
  (let* ((u1 (get-random-exclusive-0 1.0))
         (u2 (random 1.0))
         (θ (* pi 2 u2))
         (magnitude (* σ (sqrt (* -2.0 (log u1)))))
         (z0 (+ (* magnitude (cos θ)) μ)))
    ;; We only need one of these, and I can't find if
    ;; lisp has implemented a function to get both at the same time (soft FSINCOS)
    ;; (z1 (+ (* magnitude (sin θ)) μ1)))
    z0))

;; From Claude
(defun beta-distribution-sample (alpha beta)
  "Sample from a Beta distribution with parameters alpha and beta.
   Uses the Marsaglia and Tsang method."
  (when (or (<= alpha 0) (<= beta 0))
    (error "Alpha and beta parameters must be positive"))
  (cond
    ;; Use direct method for specific parameter cases
    ((= alpha 1.0) (expt (random 1.0) (/ 1.0 beta)))
    ((= beta 1.0) (- 1.0 (expt (random 1.0) (/ 1.0 alpha))))
    ;; Use Marsaglia and Tsang method for general case
    (t (let ((alpha-gamma (gamma-distribution-sample alpha))
             (beta-gamma (gamma-distribution-sample beta)))
         (/ alpha-gamma (+ alpha-gamma beta-gamma))))))

(defun gamma-distribution-sample (shape)
  "Sample from a Gamma distribution with shape parameter and scale=1.
   Uses the Marsaglia and Tsang method."
  (let* ((d (- shape 1/3))
         (c (/ 1.0 (sqrt (* 3 d)))))
    (loop
      (let* ((x (generate-gaussian-sample))
             (v (expt (+ 1.0 (* c x)) 3))
             ;; Make sure u is not zero to avoid log(0) error
             (u (get-random-exclusive-0 1.0)))
        (when (< (log u) (+ (* 0.5 x x) (* d (- 1.0 v (log v)))))
          (return (* d v)))))))

;; Example usage:
(defun test-beta-distribution (alpha beta n)
  "Generate n samples from Beta(alpha, beta) and print statistics."
  (let ((samples (loop repeat n collect (beta-distribution-sample alpha beta))))
    (format t "Beta(~A, ~A) distribution statistics:~%" alpha beta)
    (format t "Mean: ~A (Expected: ~A)~%"
            (/ (reduce #'+ samples) n)
            (/ alpha (+ alpha beta)))
    (format t "Min: ~A~%" (reduce #'min samples))
    (format t "Max: ~A~%" (reduce #'max samples))
    samples))

;; Example:
;; (test-beta-distribution 2.0 5.0 1000)
