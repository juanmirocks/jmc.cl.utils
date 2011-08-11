;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 15:04:52 2008 (CEST)
;; Last-Updated: 2011-08-11
;;           By: Juan Miguel Cejuela
;;     Update #: 22

(in-package :net.ashrentum.utils.jmcejuela)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dolist-index ((var list index &optional result) &body body)
  "make dolist bonding an index variable that starts always in 0"
  (let ((glist (gensym "list")))
    `(do* ((,glist ,list (cdr ,glist))
           (,var (car ,glist) (car ,glist))
           (,index 0 (1+ ,index)))
	 ((null ,glist) ,result)
       ,@body)))

(defmacro dolist-index&sum ((var list index sum &optional result) &body body)
  "make dolist bonding and index that starts always in 0 and a sum var that accumulates the value of var"
  (let ((glist (gensym "list")))
    `(do* ((,index 0 (1+ ,index))
           (,glist ,list (cdr ,glist))
           (,var (car ,glist) (car ,glist))
           (,sum 0))
	 ((null ,glist) ,result)
       (incf ,sum ,var)
       ,@body)))

(defmacro measure (function)
  "Give the user running time in seconds of the performed function"
  (let ((time0 (gensym)))
    `(let ((,time0 (get-internal-real-time)))
       ,function
       (float (/ (- (get-internal-real-time) ,time0) 1000)))))

(define-modify-macro setif (condition if-value &optional else-value)
  (lambda (place condition if-value &optional else-value)
    (if (funcall condition place) if-value else-value))
    "If condition T setf with if-value, otherwise with else-value")

(defmacro isetf (place &optional (delta 1))
  "Incf converted into a setf. Place is multievaluated. Don't use with push, pop, etc"
  `(setf ,place (+ ,place ,delta)))

(defmacro label&time (label &body body)
  "Label the s-expression and time it"
  `(progn
    (format *trace-output* "~%--- time ~a ---~%" ,(if label label `',@body))
    (time ,@body)))

(defmacro n-times (n label &body body)
  "Time the repetition of the body n times"
  (let ((gn (gensym)))
    `(let ((,gn (1- ,n)))
       (format t "x~d ~a~%" (1+ ,gn) ,(if label label `',@body))
       (time
        (dotimes (i ,gn ,@body)
          ,@body))
       (terpri))))

(defmacro while (test &body body)
  "While T do body"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro with-gensyms ((&rest names) &body body)
  "Let various names with a gensym"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-slot-values (slots instance &body body)
  "Like with-slots but makes a let binding of the slots instead of calling internally symbol-macrolet, ie, this is faster"
  (labels ((slot-bind (name-var name-slot instance)
                 (list name-var (list 'slot-value instance name-slot))))
    (let ((inst (gensym)))
      `(let* ,(cons (list inst instance)
                    (loop for bind in slots
                       collect (cond
                                 ((atom bind) (slot-bind bind `',bind inst))
                                 ((and (symbolp (first bind)) (symbolp (second bind)))
                                  (slot-bind (first bind) `',(second bind) inst))
                                 (t (error "in with-slot-values: slot entry ~a must be a symbol" bind)))))
         ,@body))))

(defmacro with-typed-slot-values (slots instance types-list &body body)
  "If types-list is nil, performs exactly as with-slot-values. Otherwise this alist will be used to write type declarations for the slots"
  (let ((inst (gensym)) (types (if (symbolp types-list) (symbol-value types-list) types-list)) (type-declarations))

    (labels ((declare-type (types-list variable slot)
               (let* ((var variable) (type (assoc (second slot) types-list)))
                 (when type (push `(declare (,(cdr type) ,var)) type-declarations))))
             (slot-bind (name-var name-slot instance)
               (declare-type types name-var name-slot)
               (list name-var (list 'slot-value instance name-slot))))
      `(let* ,(cons (list inst instance)
                    (loop for bind in slots
                       collect (cond
                                 ((atom bind) (slot-bind bind `',bind inst))
                                 ((and (symbolp (first bind)) (symbolp (second bind)))
                                  (slot-bind (first bind) `',(second bind) inst))
                                 (t (error "in with-slot-values: slot entry ~a must be a symbol" bind)))))
         ,@type-declarations
         ,@body))))

(defmacro mac (expr)
  "Macroexpand-1 pretty print, short access. The expression must be not quoted"
  `(pprint (macroexpand-1 ',expr)))

(defmacro nil! (var)
  "Set the variable to nil"
  `(setf ,var nil))

(define-modify-macro *= (delta)
  *
  "Like incf but a multiplier. The same as in C")

(defmacro foo (stream filename force &body body)
  "File Open for Ouput. Bind a stream to write in"
  `(with-open-file (,stream ,filename :direction :output :if-does-not-exist :create
                            :if-exists ,(if force :supersede :rename))
     ,@body))

(defmacro for ((var count counter &optional result) &body body)
  "Intended to be like the C for
	var: var to iterate or (var initial-value)
	count: limit
	counter: amount to add every iteration to var
	result: optional given value"
  (let ((v) (init 0))
    (if (atom var)
        (setq v var init 0)
        (setq v (car var) init (second var)))
    (if (numberp count)
        `(do ((,v ,init (+ ,v ,counter)))
             ((> ,v ,count) ,result)
           (declare (type fixnum ,v))
           ,@body)
        (let ((c (gensym)))
          `(do ((,v ,init (+ ,v ,counter))
                (,c ,count))
               ((> ,v ,c) ,result)
             (declare (type fixnum ,v)
                      (type fixnum ,c))
             ,@body)))))

(defmacro time-elapsed (first-reference)
  "Time elapsed in seconds from a first reference of get-internal-real-time"
  `(float (/ (- (get-internal-real-time) ,first-reference) 1000.0)))

(defmacro position-not (item sequence &key from-end (start 0) end key)
  "Complement of position"
  (let ((x (gensym)))
    `(position-if-not
      #'(lambda (,x) (equalp ,item ,x)) ,sequence
      :from-end ,from-end :start ,start :end ,end :key ,key)))

(defmacro %100 (numerator denominator &optional (output :string))
  "Gives the percentage reprenting the quantity of numerator divided by denominator"
  `(ecase ,output
     (:number (float (* 100 (/ ,numerator ,denominator))))
     (:string (write-to-string (float (* 100 (/ ,numerator ,denominator)))))))

(defmacro dotimes-nested (((&rest vars) &optional result) &body body)
  "Dotimes-nested (dotimes-nested ((i 10 j 8) result) body)"
  (labels ((aux (vars body)
             (if (and (consp vars) (consp (cdr vars)) (null (cddr vars)))
                 `(dotimes (,(car vars) ,(cadr vars) nil)
                    ,@body)
                 `(dotimes (,(car vars) ,(cadr vars) nil)
                    ,(aux (cddr vars) body)))))
    `(dotimes (,(car vars) ,(cadr vars) ,result)
       ,(aux (cddr vars) body))))

