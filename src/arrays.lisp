;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To work with arrays, and lists, and put them in a standard form for computation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-single-float-list (list &optional result)
  "Takes a list and turns it into a single-float array"
  (cond ((null list)
         (reverse result))
        ((consp (car list))
         (list-to-single-float-list (cdr list)
                                     (push (list-to-single-float-list (car list))
                                           result)))
        (t
         (list-to-single-float-list (cdr list)
                                     (push (* (car list) 1.0)
                                           result)))))

;; Do this soon...
;; (defun convert-array-to-single-float (array)
;;   "Converts an array to use single-floats instead of whatever it has."
;;   (let* ((dimensions (array-dimensions array))
;;          (depth (- (length dimensions) 1))
;;          (new-array (make-array (list (array-dimension array 0) depth)
;;                                 :element-type 'single-float)))
;;     (loop for j below ))

(defun list-depth (list &optional (depth 0))
  "Find the depth of a list matrix"
  (if (not (listp list))
      depth
      (list-depth (car list) (+ depth 1))))

(defun list-dimensions (list &optional (depth nil))
  "Retrieves the dimensions of a list. depth is optional."
  (let ((depth (if (null depth)
                   (list-depth list)
                   depth)))
    (loop repeat depth
          collect (length list)
          do (setf list (car list)))))

(defun list-to-array-single-float (list &optional (depth nil))
  "Converts an list or square list of single floats to an array or matrix"
  (let ((depth (if (null depth)
                   (list-depth list)
                   depth)))
    (make-array (list-dimensions list depth)
                :initial-contents list
                :element-type 'single-float)))

(defun my-array (input)
  "Simplified function to make inputs into single-float arrays. This should be
  the primary way to make arrays."
  (cond ((typep input '(array single-float))
         input)
        ((typep input '(array))
         (format t "Array, but not single-float")
         input)
        ((listp input)
         ;; it may seem silly, but if I don't just convert them all,
         ;; I would have to check them all..
         (list-to-array-single-float (list-to-single-float-list input)))))

(defun list-content-type (list &optional)
  "Find the content type of the first element in the list"
  (if (not (consp list))
      (type-of list)
      (list-content-type (car list))))

(defun list-to-2d-array (list)
  "Convert from a list to a two dimensional array.
  This assumes the list comes in as a 2d matrix, and the values are floats."
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list
              :element-type 'single-float))

;; array-to-list generalizes this, but this can be optimized easier.
(defun 2d-array-to-list (array)
  "Convert from a two dimensional array to a list"
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun list-to-array-not-single-float (list &optional (depth nil))
  (let ((depth (if (null depth)
                   (list-depth list)
                   depth)))
    (make-array (list-dimensions list depth)
                :initial-contents list)))

(defun list-to-array (list &optional (depth nil))
  "Converts a list to an array.
   Generic function will call other functions to calculate."
  (if (eql (list-content-type list) 'single-float)
      (list-to-array-single-float list depth)
      (list-to-array-not-single-float list depth)))

(defun array-to-list (array)
  "Takes any generic array or matrix and converts it to a list."
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

;; Testing to make sure my-array works as expected, and the dimensions come back as expected.
(defvar 1d-array (my-array '(1 1 1)))
(defvar 2d-array (my-array '((1 1 1)
                             (2 2 2)
                             (3 3 3))))
(defvar 3d-array (my-array '(((1 1 1) (2 2 2) (3 3 3))
                             ((1 1 1) (2 2 2) (3 3 3))
                             ((1 1 1) (2 2 2) (3 3 3)))))
(defvar 4d-array (my-array '((((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                             (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                             (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3))))))
(defvar 5d-array (my-array '(((((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3))))
                             ((((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3))))
                             ((((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))
                              (((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)) ((1 1 1) (2 2 2) (3 3 3)))))))
