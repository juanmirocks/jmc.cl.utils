;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 15:15:32 2008 (CEST)
;; Last-Updated: 2011-08-11
;;     Update #: 124

(in-package :net.ashrentum.utils.jmcejuela)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro arefl (array is)
  "Access array by a list of indeces"
  `(apply #'aref (cons ,array ,is)))

(defun (setf arefl) (value array is)
  (setf (arefl array is) value)) ;TODO somehow create setf function for arefl

(defun array-powers (x)
  (let ((dim (if (listp x) x (array-dimensions x))))
    (labels ((rec (l acc)
               (if (null l) (reverse acc) (rec (cdr l) (cons (reduce #'* (cdr l)) acc)))))
      (rec dim nil))))

(defun refa (pos powers)
  "Return list of indeces that correspond to absolut position 'pos' of implicit array (referenced by powers)"
  (labels ((rec (l pos acc)
             (cond
               ((null l) (reverse acc))
               ((zerop pos) (rec (cdr l) 0 (cons 0 acc)))
               (t (let ((quotient (floor (/ pos (car l))))
                        (remainder (rem pos (car l))))
                    (rec (cdr l) remainder (cons quotient acc)))))))
    (rec powers pos nil)))

(defmacro arefa (array pos &optional (powers (array-powers array)))
  "Access array by absolute position"
  `(arefl ,array (refa ,pos ,powers)))

(defun areduce (fun array)
  "Array reduce"
  (let ((powers (array-powers array))
        (acc (reduce fun nil)))
    (dotimes (i (array-total-size array) acc)
      (setf acc (funcall fun acc (arefa array i powers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-typed-array (dimensions type initial-element)
  "Make a typed array, specifying the dimensions, type and initial-element"
  `(make-array ,dimensions :element-type ,type :initial-element (coerce ,initial-element ,type)))

(defun transpose (matrix)
  "Transpose the matrix"
  (declare (optimize (speed 3) (safety 0)))
  (let ((out (make-array (array-dimensions matrix) :element-type (array-element-type matrix))))
    (dotimes (i (the fixnum (array-dimension matrix 0)) out)
      (dotimes (j (the fixnum (array-dimension matrix 1)))
        (setf (aref out i j) (aref matrix j i))))))

(defun make-random-vector (dimension range &optional (element-type 'single-float))
  "Generates a numerical vector of type element-type with random values. The range is between 0 and range"
  (let* ((zero (coerce 0 element-type))
         (range (coerce range element-type))
         (out (make-array dimension :element-type element-type :initial-element zero)))
    (dotimes (i dimension out)
      (setf (aref out i) (random range)))))

(defun make-random-matrix (dimensions range &optional (element-type 'single-float))
  "Generates a numerical matrix of type element-type with random values. The range is between 0 and range"
  (let* ((zero (coerce 0 element-type))
         (range (coerce range element-type))
         (out (make-array dimensions :element-type element-type :initial-element zero))
         (dim1 (first dimensions))
         (dim2 (second dimensions)))
    (dotimes (i dim1 out)
      (dotimes (j dim2 out)
        (setf (aref out i j) (random range))))))

(defun !combine-float-matrices (matrix1 matrix2 alpha &optional (element-type 'single-float) (let-zero-be-zero nil))
  "Combine 2 float matrices whose values range from 0 to 1. Alpha is the level of confidence in matrix1, ie, the element of a resultant value will be value = matrix1_value * alpha + matrix2_value * (- 1 alpha). Destructive function **"
  (let* ((dim1 (array-dimension matrix1 0))
         (dim2 (array-dimension matrix1 1))
         (alpha (coerce alpha element-type))
         (beta (coerce (- 1 alpha) element-type)))
    (dotimes (i dim1 matrix1)
      (dotimes (j dim2)
        (setf (aref matrix1 i j) (if (and let-zero-be-zero (zerop (aref matrix1 i j)))
                                    (aref matrix1 i j)
                                    (+ (* (aref matrix1 i j) alpha) (* (aref matrix2 i j) beta))))))))

(defun !combine-float-vectors (vec1 vec2 alpha &optional (element-type 'single-float) (let-zero-be-zero nil))
  "Combine 2 float vectors whose values range from 0 to 1. Alpha is the level of confidence in vec1, ie, the element of a resultant value will be value = vec1_value * alpha + vec2_value * (- 1 alpha). Destructive function **"
  (let* ((size (min (length vec1) (length vec2)))
         (alpha (coerce alpha element-type))
         (beta (coerce (- 1 alpha) element-type)))
    (dotimes (i size vec1)
      (setf (aref vec1 i) (if (and let-zero-be-zero (zerop (aref vec1 i)))
                              (aref vec1 i)
                              (+ (* (aref vec1 i) alpha) (* (aref vec2 i) beta)))))))

(defun vector= (vec1 vec2 &key (test #'eql))
  "Compare 2 vectors with test function"
  (declare (vector vec1 vec2))
  (let ((size1 (length vec1))
        (size2 (length vec2)))
    (declare (fixnum size1 size2))
    (when (= size1 size2)
      (dotimes (i size1 T)
        (unless (funcall test (aref vec1 i) (aref vec2 i))
          (return nil))))))

(defun matrix= (matrix1 matrix2 &key (test #'eql))
  "Compare 2 matrices with test function"
  (declare ((simple-array) matrix1 matrix2))
  (let ((dim11 (array-dimension matrix1 0))
        (dim12 (array-dimension matrix1 1))
        (dim21 (array-dimension matrix2 0))
        (dim22 (array-dimension matrix2 1)))
    (declare (fixnum dim11 dim12 dim21 dim22))
    (when (and (= dim11 dim21) (= dim12 dim22))
      (dotimes (i dim11 T)
        (dotimes (j dim12)
          (unless (funcall test (aref matrix1 i j) (aref matrix2 i j))
            (return nil)))))))

(defun vector-from-matrix (matrix nthrow)
  "Get the nth row of the quadratic matrix converted into a vector of the same type of the matrix"
  (let* ((dim2 (array-dimension matrix 1))
         (out (make-array dim2 :element-type (array-element-type matrix))))
    (dotimes (i dim2 out)
      (setf (aref out i) (aref matrix nthrow i)))))

(defun !normalize-vector (vector)
  "DEPRECATED, use !normalize-array -- **Destructive** -- Normalize the values of a float vector."
  (let ((cero (coerce 0 (array-element-type vector)))
        (factor (reduce #'+ vector))
        (len (length vector)))
    (dotimes (i len vector)
      (setf (aref vector i)
            (if (zerop factor)
                cero
                (/ (aref vector i) factor))))))

(defun !normalize-2dmatrix-by-row (matrix)
  "**Destructive** -- Normalize the values of a float matrix. "
  (destructuring-bind (f s) (array-dimensions matrix)
    (let* ((cero (coerce 0 (array-element-type matrix))) (factor cero))
      (declare (real factor))
         (dotimes (i f matrix)
           (setf factor cero)
           (dotimes (j s)
             (incf factor (aref matrix i j)))
           (dotimes (j s)
             (setf (aref matrix i j)
                   (if (zerop factor)
                       cero
                       (/ (aref matrix i j) factor))))))))

(defun !normalize-3dmatrix-by-row (matrix)
  "**Destructive** -- Normalize the values of a float 3d matrix by row."
  (destructuring-bind (s1 s2 s3) (array-dimensions matrix)
    (let* ((zero (coerce 0 (array-element-type matrix)))
           (zustandssumme zero))
      (declare (real zustandssumme))
      (dotimes (i s1 matrix)
        (setf zustandssumme zero)
        (dotimes (j s2)
          (dotimes (z s3)
            (incf zustandssumme (aref matrix i j z))))
        (dotimes (j s2)
          (dotimes (z s3)
            (setf (aref matrix i j z)
                  (if (zerop zustandssumme)
                      zero
                      (/ (aref matrix i j z) zustandssumme)))))))))

(defun !normalize-array (array)
  (let ((zustandssumme (areduce #'+ array)))
    (if (zerop zustandssumme)
        array
        (dotimes (i (array-total-size array) array)
          (setf (arefa array i) (/ (arefa array i) zustandssumme))))))

(defmacro accum-array (array no-dimensions element-type)
  "Give the accumulative float array of any dimension"
  (macrolet ((bucle (i &body body)
               ``(dotimes (,(car (push (gensym) indexes)) (nth ,,i ,dims))
                   ,,@body ,(deep-array (the fixnum (incf i)) (the fixnum (decf n)) indexes
                                        a-in dims element-type a-out accu))))
    (labels ((deep-array (i n indexes a-in dims element-type a-out accu)
               (cond
                 ((zerop n) (let ((r (reverse indexes)))
                              `(progn  (the ,element-type (incf ,accu (aref ,a-in ,@r)))
                                       (setf (aref ,a-out ,@r) ,accu))))
                 ((= 2 n) (bucle i `(setf ,accu (coerce 0 ',element-type))))
                 (t (bucle i)))))

      (with-gensyms (dims a-in a-out accu)
        (let ((gn no-dimensions))
          `(let* ((,a-in ,array)
                  (,dims (array-dimensions ,a-in))
                  (,a-out (make-array ,dims
                                      :element-type ',element-type
                                      :initial-element (coerce 0 ',element-type)
                                      :adjustable (adjustable-array-p ,a-in)))
                  (,accu (coerce 0 ',element-type)))
             (declare (optimize (speed 3) (safety 0)) ((simple-array ,element-type) ,a-in ,a-out) (,element-type ,accu))
             ,(deep-array 0 gn nil a-in dims element-type a-out accu)
             ,a-out))))))


(defun copy-matrix (matrix)
  "DEPRECATED, use `copy-array` -- Copy a matrix"
  (let* ((dims (array-dimensions matrix)) (dimx (car dims)) (dimy (cadr dims))
         (out (make-array dims :element-type (array-element-type matrix))))
    (dotimes (i dimx out)
      (dotimes (j dimy)
        (setf (aref out i j) (aref matrix i j))))))

(defun copy-array (array &key (element-type (array-element-type array))
                              (fill-pointer (and (array-has-fill-pointer-p array)
                                                 (fill-pointer array)))
                              (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
 (let* ((dimensions (array-dimensions array))
        (new-array (make-array dimensions
                               :element-type element-type
                               :adjustable adjustable
                               :fill-pointer fill-pointer)))
   (dotimes (i (array-total-size array))
     (setf (row-major-aref new-array i)
           (row-major-aref array i)))
   new-array))
