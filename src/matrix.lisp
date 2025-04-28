;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to work on matrices ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transpose functions
(defun matrix-transpose (matrix &optional (row 0) (column 0) (new-matrix nil))
  "Transpose a matrix: only accepts arrays of 'single-float"
  (declare (type (array single-float) matrix))
  (if (null new-matrix)
      (setq new-matrix
            (make-array
             (list (array-dimension matrix 1) (array-dimension matrix 0))
             :element-type 'single-float)))
  (cond ((and (= row (- (array-dimension matrix 0) 1))
              (= column (- (array-dimension matrix 1) 1)))
         (setf (aref new-matrix column row) (aref matrix row column))
         new-matrix)
        ((< column (- (array-dimension matrix 1) 1))
         (setf (aref new-matrix column row) (aref matrix row column))
         (matrix-transpose-array matrix row (+ column 1) new-matrix))
        (t ;; (< row (- (array-dimension matrix 0) 1))
         (setf (aref new-matrix column row) (aref matrix row column))
         (matrix-transpose-array matrix (+ row 1) 0 new-matrix))))

;; There are multiple ways to multiply something against a matrix:
;; Matrix-A * Matrix-B - Matrix Multiplication
;; Matrix * Vector
;; Vector * Matrix
;; Vector * Vector - special case of dot product.
;; Scalar * Matrix - Matrix Scalar Multiplication

;; for vectors
(defun scalar-vector-multiplication-vector (scalar vector)
  "multiply a vector by a scalar"
  (let ((new-vector (make-array (length vector))))
    (dotimes (pos (length vector))
      (setf (aref new-vector pos) (* scalar (aref vector pos))))
    new-vector))

;; Needs to be more optimized, but this is what I have now.
(defun scalar-matrix-multiplication-vector (scalar matrix)
  "multiply a matrix by a scalar"
  (let ((new-matrix (make-array (list (array-dimension matrix 0) (array-dimension matrix 1)))))
    (dotimes (pos-x (array-dimension matrix 0))
      (dotimes (pos-y (array-dimension matrix 1))
        (setf (aref new-matrix pos-x pos-y) (* scalar (aref matrix pos-x pos-y)))))
    new-matrix))

(defun matrix-multiply (matrix1 matrix2 &optional (row1 0) (column1 0) (column2 0) (new-matrix nil))
  "Multiplies two matrices: only accepts arrays of 'single-float"
  (declare (type (array single-float) matrix1 matrix2))
  (assert (= (array-dimension matrix1 1) (array-dimension matrix2 0)))
  (if (null new-matrix)
      (setq new-matrix
            (make-array
             ;; should be the size of (rows of matrix1 x columns of matrix2)
             (list (array-dimension matrix1 0) (array-dimension matrix2 1))
             :element-type 'single-float)))
  (setf (aref new-matrix row1 column2)
        (+ (* (aref matrix1 row1 column1)
              (aref matrix2 column1 column2))
           (aref new-matrix row1 column2)))
  (cond ((and (= row1 (- (array-dimension matrix1 0) 1))
              (= column1 (- (array-dimension matrix1 1) 1))
              (= column2 (- (array-dimension matrix2 1) 1)))
         new-matrix)
        ((< column1 (- (array-dimension matrix1 1) 1))
         (matrix-multiply matrix1 matrix2 row1 (+ column1 1) column2 new-matrix))
        ((< column2 (- (array-dimension matrix2 1) 1))
         (matrix-multiply matrix1 matrix2 row1 0 (+ column2 1) new-matrix))
        (t ;; (< row (- (array-dimension matrix 0) 1))
         (matrix-multiply matrix1 matrix2 (+ row1 1) 0 0 new-matrix))))

(defun dot-product (array1 array2 &optional (sizes-good nil) (running-total 0.0) (position 0))
  "Compute a dot product of two single dimensional arrays of single-floats."
  (declare (optimize speed)
           (type (simple-array single-float (*)) array1 array2)
           (type (single-float) running-total)
           (type (alexandria:array-index) position))
  (if (not sizes-good)
      (let ((size1 (length array1))
            (size2 (length array2)))
        (assert (= size1 size2))))
  (if (= position (length array1))
      running-total
      (dot-product array1
                   array2
                   t
                   (+ running-total (* (aref array1 position) (aref array2 position)))
                   (+ position 1))))

;; TODO: Make recursive, optimize for arrays, and make a list wrapper, and the generic wrapper
(defun matrix-where (matrix condition if-true if-false)
  (declare (optimize speed)
           (type (simple-array single-float) matrix))
  (let ((new-matrix (make-array (array-dimensions matrix1)
                                :element-type 'single-float)))
  (let ((rows (length matrix))
        (columns (length (car matrix)))
        (return nil))
    (loop for row below rows
          do (let ((current-row nil))
               (loop for column below columns
                     do (if (funcall condition (nth column (nth row matrix)))
                            (push if-true current-row)
                            (push if-false current-row)))
               (push (reverse current-row) return)))
    (reverse return))))

(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (- (length dimensions) 1))
         (indices    (make-list (+ depth 1) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions) do
                 (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                                 (recurse (+ n 1))))))
      (recurse 0))))

;; Recursive Helper
(defun matrix-traverse (matrix &optional (i 0) (j 0))
  "Traverse a 2d matrix recursively. Used for reference, unless
  I want to print the arrays differently."
  (cond ((and (= i (- (length matrix) 1)) (= j (- (length (car matrix)) 1)))
         (format t "[~D,~D]:~D~% " i j (nth j (nth i matrix))))
        ((< j (- (length (car matrix)) 1))
           (format t "[~D,~D]:~D, " i j (nth j (nth i matrix)))
           (matrix-traverse matrix i (+ j 1)))
        (t ;; (< i (- (length matrix) 1))
           (format t "[~D,~D]:~D, " i j (nth j (nth i matrix)))
           (matrix-traverse matrix (+ i 1) 0))))

(defun matrix-traverse-n-dimensions (matrix)
  "Will traverse a matrix of any size.
  Tested up to 5 dimensions, but the logic holds for many many more."
  (declare (type (array single-float) matrix))
  (let* ((dimensions (array-dimensions matrix))
        (indexes (make-list (length dimensions) :initial-element 0)))
    (format t "~a:~a:~a~%" indexes dimensions (apply #'aref matrix indexes))
    (cond ((equal indexes dimensions)
           (format t "Done"))
          (t (let ((done nil)
                   (current-index (- (length indexes) 1)))
               (loop while (not done) do
                 (setf (nth current-index indexes) (+ (nth current-index indexes) 1))
                 (if (= (nth current-index indexes) (nth current-index dimensions))
                     (progn
                       (setf (nth current-index indexes) 0)
                       (setf current-index (- current-index 1))
                       (if (< current-index 0) (setf done t)))
                     (progn
                       (setf current-index (- (length indexes) 1))
                       (format t "~a:~a:~a ~%" indexes dimensions (apply #'aref matrix indexes))))))))))

(defun matrix-conversion (matrix conversion)
  "Apply a conversion to each value in the matrix. "
  (declare (type (array single-float) matrix))
  (let* ((dimensions (array-dimensions matrix))
         (indexes (make-list (length dimensions) :initial-element 0))
         (new-matrix (make-array (array-dimensions matrix) :initial-element 0.0 :element-type 'single-float))
         (done nil)
         (current-index (- (length indexes) 1)))
    (setf (apply #'aref new-matrix indexes)
          (funcall conversion (apply #'aref matrix indexes)))
    (loop while (not done) do
      (setf (nth current-index indexes) (+ (nth current-index indexes) 1))
      (if (= (nth current-index indexes) (nth current-index dimensions))
          (progn
            (setf (nth current-index indexes) 0)
            (setf current-index (- current-index 1))
            (if (< current-index 0) (setf done t)))
          (progn
            (setf current-index (- (length indexes) 1))
            (setf (apply #'aref new-matrix indexes)
                  (funcall conversion (apply #'aref matrix indexes))))))
    new-matrix))

(defun matrix-where (matrix condition if-true if-false)
  (matrix-conversion matrix
                     #'(lambda (x) (if (funcall condition x)
                                       (* 1.0 if-true)
                                       (* 1.0 if-false)))))
