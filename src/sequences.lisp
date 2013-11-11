;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 15:16:28 2008 (CEST)
;; Last-Updated: 2011-08-11
;;     Update #: 11

(in-package :net.ashrentum.utils.jmcejuela)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leng=1 (list)
  "Return T if length of list is 1, otherwise NIL. Checks only the cons and cdr"
  (and (consp list) (null (cdr list))))

(defun leng=2 (list)
  "Return T if length of list is 2, otherwise NIL. Checks only car, cadr, cddr"
  (and (consp list) (consp (cdr list)) (null (cddr list))))

(defun flatten (tree)
  "Flatten a tree"
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((atom tree) (cons tree acc))
                   (t (rec (car tree) (rec (cdr tree) acc))))))
    (rec tree nil)))

(defun longer (x y)
  "T if the list y is longer than x. Doing the minimum calculations"
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defmacro make-list-meval (size initial-element)
  "Exactly like make-list but the initial-element must be given and this will be evaluated for every position"
  (let ((gsize (gensym)) (glist (gensym)) (gi (gensym)))
    `(do ((,gsize ,size)
          (,glist nil)
          (,gi 0 (1+ ,gi)))
         ((= ,gi ,gsize) (reverse ,glist))
       (push ,initial-element ,glist))))

(defun similarity (seq1 seq2 &optional (test 'equal))
  "Normalized similarity of two sequences"
  (let ((len1 (length seq1))
        (len2 (length seq2))
        (score 0))
    (declare (fixnum len1 len2 score))
    (dotimes (i (min len1 len2) (values (float (/ score i)) score i))
      (when (funcall test (aref seq1 i) (aref seq2 i))
        (incf score)))))

(defun list->sequence (list sequence-type)
  "Convert the list into a sequence"
  (concatenate sequence-type list))

(defun sequence->list (sequence)
  "Convert the sequence into a list"
  (concatenate 'list sequence))

(defun matrix->list (matrix)
  "Convert the matrix into a list, tree"
  (declare ((simple-array) matrix))
  (let ((list))
    (dotimes (i (array-dimension matrix 0) (nreverse list))
      (push
       (let ((sublist))
         (dotimes (j (array-dimension matrix 1) (nreverse sublist))
           (push (aref matrix i j) sublist)))
       list))))

(defun array->list (array)
  "Convert array of any dimension to a list"
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                  do (setf (nth n indices) j)
                  collect (if (= n depth)
                              (apply #'aref array indices)
                              (recurse (1+ n))))))
      (recurse 0))))

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
     collect n))
