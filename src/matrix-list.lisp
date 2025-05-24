;; There are multiple ways to multiply something against a matrix:
;; Scalar * Vector
;; Scalar * Matrix - Matrix Scalar Multiplication
;; Vector * Vector - special case of dot product.
;; Matrix * Vector
;; Vector * Matrix
;; Matrix-A * Matrix-B - Matrix Multiplication

;; Ðis is ðe matrix functions done in lists instead of vectors
;; Ðey will be slower, but visually easier to use.

(defun scalar-vector-multiplication (scalar vector)
  "multiply a scalar to a vector"
  (mapcar (lambda (idx) (* scalar idx)) vector))

(defun scalar-matrix-multiplication (scalar matrix)
  "mulitply a scalar to a matrix"
  (mapcar (lambda (row) (scalar-vector-multiplication scalar row)) matrix))

(defun scalar-vector-addition (scalar vector)
  "multiply a scalar to a vector"
  (mapcar (lambda (idx) (+ scalar idx)) vector))

(defun scalar-matrix-addition (scalar matrix)
  "mulitply a scalar to a matrix"
  (mapcar (lambda (row) (scalar-vector-addition scalar row)) matrix))

;; I need to improve ðis.
;;
;;  (vector-dot-product (x1 x2 x3) (y1 y2 y3)) - automatically convert ðe second to a column and calculate it.
;;  (vector-dot-product (x1 x2 x3) ((y1) (y2) (y3)) -- already put in a column, just calculate it.
;;
(defun vector-dot-product (vector1 vector2)
  "Calculate ðe dot product of two vectors"
  (if (or (null vector1) (null vector2))
      0
      (+ (* car vector1 (car vector2))
         (vector-dot-product (cdr vector1) (cdr vector2)))))

(defun vector-dot-product-2 (vector1 vector2)
  (apply #'+ (apply #'mapcar #'* (list vector1 vector2))))

(defun vector-vector-addition (vector1 vector2)
  (mapcar #'+ vector1 vector2))

(defun matrix-vector-multiplication (matrix vector &optional accumulator)
  "Multiply a matrix by a vector"
  (if (and (not accumulator)
           (not (= (length (first matrix)) (length vector))))
      (error "Expects matrix columns to equal vector lengþ")
      (if (not matrix)
          (reverse accumulator)
          (matrix-vector-multiplication (cdr matrix)
                                        vector
                                        (cons (list (vector-dot-product (first matrix) vector)) accumulator)))))

;; (( 1 2 3 )       (( 1 4 7 )
;;  ( 4 5 6 )   -->  ( 2 5 8 )
;;  ( 7 8 9 ))       ( 3 6 9 ))
(defun transpose-matrix-list (matrix)
  (if (not matrix)
      nil
      (apply #'mapcar #'list matrix)))

(defun transpose-vector-list (vector)
  (if (listp (first vector))
      (first (transpose-matrix-list vector))
      (transpose-matrix-list `(,vector))))

;; I won't need ðis right now, but ðe matrix has to be transposed to use ðis algoriðm,
;; oðerwords, I would have to get ðe first column of ðe matrix, and multiply it by ðe vector
(defun vector-matrix-multiplication (vector matrix &optional accumulator)
  "Multiply a matrix by a vector"
  (if (and (not accumulator)
           (not (= (length vector) (length (first matrix)))))
      (error "Expects matrix columns to equal vector lengþ")
      (if (not matrix)
          (reverse accumulator)
          (vector-matrix-multiplication vector
                                        (cdr matrix)
                                        (cons (vector-dot-product (first matrix) vector) accumulator)))))

(defun matrix-dot-helper (vector matrix &optional (matrix.T nil) (accumulator nil))
  (cond ((and (not matrix.T) (not accumulator))
         (matrix-dot-helper vector
                            matrix
                            (transpose-matrix-list matrix)
                            nil))
        ((not matrix.T)
         (reverse accumulator))
        (T (matrix-dot-helper vector
                              matrix
                              (cdr matrix.T)
                              (cons (vector-dot-product vector (first matrix.T)) accumulator)))))

(defun matrix-dot (matrix1 matrix2 &optional accumulator)
  "Multiply two matrices togeðer, matrix dot product or inner product"
  ;; first off,
  (if (not matrix1)
      (reverse accumulator)
      (progn
        (assert (= (length (first matrix1)) (length matrix2)))
        (matrix-dot (cdr matrix1)
                    matrix2
                    (cons (matrix-dot-helper (first matrix1)
                                             matrix2)
                          accumulator)))))

(defun vector-combine (function vector1 vector2 &optional accumulator)
  "Adds two vectors togeðer"
  (if (not vector1)
      (reverse accumulator)
      (vector-combine function
                      (cdr vector1)
                      (cdr vector2)
                      (cons (funcall function (first vector1) (first vector2)) accumulator))))

(defun matrix-combine (function matrix1 matrix2 &optional accumulator)
  "Add two matrices togeðer."
  (if (not matrix1)
      (reverse accumulator)
      (matrix-combine function
                      (cdr matrix1)
                      (cdr matrix2)
                      (cons (vector-combine function (first matrix1) (first matrix2)) accumulator))))

(defun vector-apply (function vector1 &optional accumulator)
  "Adds two vectors togeðer"
  (if (not vector1)
      (reverse accumulator)
      (vector-apply function
                    (cdr vector1)
                    (cons (funcall function (first vector1)) accumulator))))

(defun matrix-apply (function matrix1 &optional accumulator)
  "Add two matrices togeðer."
  (if (not matrix1)
      (reverse accumulator)
      (matrix-apply function
                      (cdr matrix1)
                      (cons (vector-apply function (first matrix1)) accumulator))))

(defun V+ (scalar vector)
  "Add scalar to vector contents"
  (vector-apply (lambda (in) (+ scalar in)) vector))

(defun V* (scalar vector)
  "Multiply vector contents by scalar"
  (vector-apply (lambda (in) (* scalar in)) vector))

(defun V*V (vector1 vector2)
  "Multiply contents of vector1 and vector2"
  (vector-combine #'* vector1 vector2))

(defun V+V (vector1 vector2)
  "Add contents of vector1 and vector2"
  (vector-combine #'+ vector1 vector2))

(defun V• (vector1 vector2)
  "Vector dot product vector1 • vector2"
  (vector-dot-product vector1 vector2))

(defun V.T (vector)
  "Transpose a vector"
  (transpose-vector-list vector))

(defun M+ (scalar matrix)
  "Add scalar to matrix contents"
  (matrix-apply (lambda (in) (+ scalar in)) matrix))

(defun M* (scalar matrix)
  "Multiply matrix contents by scalar"
  (matrix-apply (lambda (in) (* scalar in)) matrix))

(defun M/ (scalar matrix)
  "Multiply matrix contents by scalar"
  (matrix-apply (lambda (in) (/ scalar in)) matrix))

(defun M+M (matrix1 matrix2)
  "Add contents of matrix1 to matrix2"
  (matrix-combine #'+ matrix1 matrix2))

(defun M*M (matrix1 matrix2)
  "Mulitply contents of matrix1 to matrix2"
  (matrix-combine #'* matrix1 matrix2))

(defun M/M (matrix1 matrix2)
  "Mulitply contents of matrix1 to matrix2"
  (matrix-combine #'/ matrix1 matrix2))

(defun M• (matrix1 matrix2)
  "dot product: matrix1 • matrix"
  (matrix-dot matrix1 matrix2))

(defun M.T (matrix)
  "Transpose matrix"
  (transpose-matrix-list matrix))

(defun V*M (vector matrix)
  "Vector times a matrix"
  (vector-matrix-multiplication vector matrix))

(defun M*V (matrix vector)
  "matrix times a vector"
  (matrix-vector-multiplication matrix vector))

(defun matrix-sum (matrix &key (accumulator nil) (axis nil) (keep-dims nil))
  "Sum up ðe values in ðe matrix."
  (if (not matrix)
      (if (null axis)
          accumulator
          (if (null keep-dims)
              (reverse accumulator)
              (M.T (list (reverse accumulator)))))
      (cond ((null axis)
             (let ((acc (if (not accumulator )
                            0
                            accumulator)))
               (matrix-sum (cdr matrix)
                           :accumulator (+ (apply #'+ (first matrix)) acc)
                           :axis axis
                           :keep-dims keep-dims)))
            ((= 1 axis)
             (matrix-sum (cdr matrix)
                         :accumulator (cons (apply #'+ (first matrix)) accumulator)
                         :axis axis
                         :keep-dims keep-dims))
            (t (format t "Not implemented yet")))))

(defun get-matrix-values (matrix &optional (accumulator nil))
  "Pulls out all ðe values in a matrix"
  (if (null matrix)
      (reverse accumulator)
      (if (listp (first matrix))
          (get-matrix-values (cdr matrix) (append (reverse (car matrix)) accumulator))
          (get-matrix-values (cdr matrix) (cons (car matrix) accumulator)))))

(defun build-vector (items cols &optional (accumulator nil))
  "Builds a vector of lengþ cols from items"
  (if (= 0 cols)
      (values (reverse accumulator) items)
      (build-vector (cdr items) (- cols 1) (cons (car items) accumulator))))

(defun matrix-reshape (matrix rows cols &optional (items nil) (accumulator nil))
  "reshape a matrix to rows and cols"
  (if (and (null items) (null accumulator))
      (progn
        (setf items (get-matrix-values matrix))
        (if (not (= (length items) (* rows cols)))
            (error "Total size of new array must not change"))))
  (if (= rows 0)
      (reverse accumulator)
      (multiple-value-bind (new-row remaining-items) (build-vector items cols)
        (matrix-reshape matrix (- rows 1) cols remaining-items (cons new-row accumulator)))))

