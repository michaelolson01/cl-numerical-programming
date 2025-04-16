

;; Gaussian or Standard Normal Distribution
(defun generate-gaussian-noise (μ σ)
  "Box-Mueller gaussian sample maker"
  (let* (;; Convert these to floats for ease (doesn't seem to affect running time)
         (μ1 (if (not (typep μ 'single-float))
                 (* 1.0 μ)
                 μ))
         (σ1 (if (not (typep σ 'single-float))
                 (* 1.0 σ)
                 σ))
         ;; 0.0 to 1.0 but never exactly 0.0
         (u1 (+ 0.000000001 (random 0.999999999)))
         (u2 (random 1.0))
         (θ (* pi 2 u2))
         (magnitude (* σ1 (sqrt (* -2.0 (log u1)))))
         (z0 (+ (* magnitude (cos θ)) μ1))
         (z1 (+ (* magnitude (sin θ)) μ1)))
    (values z0 z1)))

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
      (let* ((x (normal-distribution-sample))
             (v (expt (+ 1.0 (* c x)) 3))
             (u (random 1.0)))
        (when (or (>= u 0.0)
                  (< (log u) (+ (* 0.5 x x) (* d (- 1.0 v (log v))))))
          (return (* d v)))))))

(defun normal-distribution-sample ()
  "Sample from the standard normal distribution using Box-Muller transform."
  (let ((u1 (loop for x = (random 1.0) until (> x 0.0) finally (return x)))
        (u2 (random 1.0)))
    (* (sqrt (* -2 (log u1))) (cos (* 2 pi u2)))))

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
