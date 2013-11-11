;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 20:06:36 2008 (CEST)
;; Last-Updated: 2011-08-11
;;     Update #: 11

(in-package :net.ashrentum.utils.jmcejuela)

(proclaim '(inline lin-search turn-roulette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file->vector (file element-type &key (max-size most-positive-fixnum) (skip-comments) (skip-letters))
  "Translate the content of a file into a vector, deciding to skip letters or whole lines starting with special chars."
  (declare (optimize (speed 3) (safety 0)) (fixnum max-size))
  (with-open-file (stream file)
    (let* ((size 65536) (vector (make-array size :element-type element-type :adjustable t :fill-pointer 0)))
      (declare ((vector) vector))
      (do ((i 0)
           (c (read-char stream nil nil) (read-char stream nil nil)))
          ((or (null c) (= i max-size)) vector)
        (declare (fixnum i))
        (cond
          ((member c skip-comments) (read-line stream))
          ((not (member c skip-letters))
           (unless (vector-push c vector)
             ;;notice that adjusting the vector is so expensive, but we don't know a priori the size of the info
             ;;force to pass the expected size?
             (adjust-array vector (the fixnum (+ i size)))
             (vector-push c vector))
           (incf i)))))))

(defun look-up (vector codebook)
  "Transform a indexed-vector into its meaning represented in codebook"
  (declare (optimize (speed 3) (safety 0)))
  (let* ((len (length vector))
         (labeled (make-array len :element-type (array-element-type codebook))))
    (dotimes (i len labeled)
      (setf (aref labeled i) (aref codebook (aref vector i))))))

(defun bin-search (obj vec func= func<)
  "Search the obj in the sorted vector comparing with the func functions"
  (let* ((n (length vec)) (start 0) (end n))
    (do ((mid (truncate (/ (+ start end) 2)) (truncate (/ (+ start end) 2))))
       ((>= start end) nil)
      (if (funcall func< (aref vec mid) obj)
          (setf start (1+ mid))
          (setf end mid)))
    (if (and (< start n) (funcall func= (aref vec start) obj))
        (values start t)
        (values start nil))))

;;lin-search run faster for small vectors (~40) than bin-search
;;and is more easy to translate to the case of accumulative values just changing setting func= as an actual func>
(defun lin-search (obj vec func= &optional (start 0) (end (length vec)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum start end) ((simple-array) vec))
  "Search the obj in the vector comparing with the func function"
  (loop for i of-type fixnum from start to (1- end) do
       (when (funcall func= (row-major-aref vec i) obj) ;can be used with matrices
         (return-from lin-search i)))
  nil)

;;define your own linear search as fast as possible
(defmacro def-lin-search (name obj-type vec-type fun)
  `(defun ,name (obj vec &optional (start 0) (end (length vec)))
     "Search the obj in the vector comparing with the func function"
     (declare (optimize (speed 3) (safety 0)) (inline ,name)
              (,obj-type obj) (,vec-type vec) (fixnum start end))
     (do ((i start (1+ i)))
	 ((= i end) nil)
       (declare (fixnum i))
       (when (,fun (row-major-aref vec i) obj) (return i)))))

(defun turn-roulette (vec-roulette vec-awards func= &optional (start 0) (end (length vec-roulette)))
  "Given a vector of accumulative probabilities (the last is 1), select randomly 1 position and translate into
a position of the awards given"
  (declare (optimize (speed 3) (safety 0)) (fixnum start end))
  (aref vec-awards (lin-search (random 1.0) vec-roulette func= start end)))

(defun date (&optional (date (get-universal-time)))
  (multiple-value-bind (s i h d m y) (decode-universal-time date)
    (declare (ignore s i h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)))

(defun lisparray->pythonarray (array)
  "Convert a lisp array into a python array. Both are in their string representation"
  (macrolet ((cont (new)
               `(setf out (concatenate 'string out ,new))))
    (destructuring-bind (dim0 dim1) (array-dimensions array)
      (let ((out (make-string 0)))
        (decf dim0) (decf dim1)
        (cont "[")
        (dotimes (i (1+ dim0) out)
          (cont "[")
          (dotimes (j dim1
                    (if (= i dim0)
                        (cont (format nil "~a]]" (aref array i j)))
                        (cont (format nil "~a], " (aref array i j)))))
            (cont (format nil "~a, " (aref array i j)))))))))

(defun make-hash-table-with-list (list &key (test 'eql) (rehash-size 1.5) (rehash-threshold 1) weakness synchronized)
  "Make a hash table with a list. The elements of the list are the keys and their positions the value"
  (let ((hash (make-hash-table :test test :size (length list)
                                          :rehash-size rehash-size :rehash-threshold rehash-threshold
                                          :weakness weakness :synchronized synchronized)))
    (do ((i 0 (1+ i))
         (e (car list) (car l))
         (l (cdr list) (cdr l)))
        ((null e) hash)
      (declare (fixnum i))
      (setf (gethash e hash) i))))
