;; There are multiple ways to multiply something against a matrix:
;; Scalar * Vector
;; Scalar * Matrix - Matrix Scalar Multiplication
;; Vector * Vector - special case of dot product.
;; Matrix * Vector
;; Vector * Matrix
;; Matrix-A * Matrix-B - Matrix Multiplication

;; This is the matrix functions done in lists instead of vectors
;; They will be slower, but somewhat easier to use.

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

;; I need to improve this.
;;
;;  (dot (x1 x2 x3) (y1 y2 y3)) - automatically convert the second to a column and calculate it.
;;  (dot (x1 x2 x3) ((y1) (y2) (y3)) -- already put in a column, just calculate it.
;;
(defun dot (vector1 vector2)
  "multiply a vector to a vector"
  ;; first check to make sure they are the correct dimensions
;;  (let ((vector2-a (if (numberp (first vector2) 1)
;;                       (mapcar #'list vector2)
;;                       vector2)))
  (apply #'+ (apply #'mapcar #'* (list vector1 vector2))))

(defun vector-vector-addition (vector1 vector2)
  (mapcar #'+ vector1 vector2))

(defun matrix-vector-multiplication (matrix vector &optional accumulator)
  "Multiply a matrix by a vector"
  (if (and (not accumulator)
           (not (= (length (first matrix)) (length vector))))
      (error "Expects matrix columns to equal vector length")
      (if (not matrix)
          (reverse accumulator)
          (matrix-vector-multiplication (cdr matrix)
                                        vector
                                        (cons (list (dot (first matrix) vector)) accumulator)))))

;; (( 1 2 3 )       (( 1 4 7 )
;;  ( 4 5 6 )   -->  ( 2 5 8 )
;;  ( 7 8 9 ))       ( 3 6 9 ))
(defun transpose-matrix-list (matrix)
  (if (not matrix)
      nil
      (apply #'mapcar #'list matrix)))

;; I won't need this right now, but the matrix has to be transposed to use this algorithm,
;; otherwords, I would have to get the first column of the matrix, and multiply it by the vector
(defun vector-matrix-multiplication (vector matrix &optional accumulator)
  "Multiply a matrix by a vector"
  (if (and (not accumulator)
           (not (= (length vector) (length (first matrix)))))
      (error "Expects matrix columns to equal vector length")
      (if (not matrix)
          (reverse accumulator)
          (vector-matrix-multiplication vector
                                        (cdr matrix)
                                        (cons (dot (first matrix) vector) accumulator)))))

;; should return a column if the second is just a column.
;; maybe it is because I attempted to simplify by using a transposed matrix, and the vector dot product
;; which i have the wrong algorithm for...
(defun matrix-dot (matrix1 matrix2 &optional matrix2t accumulator)
  "Multiply two matrices together, matrix dot product or inner product"
  (if (not matrix1)
      (mapcar #'list (reverse accumulator))
      (progn
        (if (not matrix2t)
            (setf matrix2t (transpose-matrix-list matrix2)))
        (matrix-dot (cdr matrix1)
                    matrix2
                    (cdr matrix2t)
                    (cons (dot (first matrix1) (first matrix2t)) accumulator)))))

(defun vector-combine (function vector1 vector2 &optional accumulator)
  "Adds two vectors together"
  (if (not vector1)
      (reverse accumulator)
      (vector-combine function
                      (cdr vector1)
                      (cdr vector2)
                      (cons (funcall function (first vector1) (first vector2)) accumulator))))

(defun matrix-combine (function matrix1 matrix2 &optional accumulator)
  "Add two matrices together."
  (if (not matrix1)
      (reverse accumulator)
      (matrix-combine function
                      (cdr matrix1)
                      (cdr matrix2)
                      (cons (vector-combine function (first matrix1) (first matrix2)) accumulator))))

(defun vector-apply (function vector1 &optional accumulator)
  "Adds two vectors together"
  (if (not vector1)
      (reverse accumulator)
      (vector-apply function
                    (cdr vector1)
                    (cons (funcall function (first vector1)) accumulator))))

(defun matrix-apply (function matrix1 &optional accumulator)
  "Add two matrices together."
  (if (not matrix1)
      (reverse accumulator)
      (matrix-apply function
                      (cdr matrix1)
                      (cons (vector-apply function (first matrix1)) accumulator))))
