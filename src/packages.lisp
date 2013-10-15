;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 19:47:10 2008 (CEST)
;; Last-Updated: 2011-08-11
;;     Update #: 11

(defpackage :net.ashrentum.utils.jmcejuela
  (:use :cl)
  (:nicknames :jmcejuela)
  (:export
   ;;;macros
   :dolist-index
   :dolist-index&sum
   :measure
   :setif
   :isetf
   :label&time
   :n-times
   :while
   :with-gensyms
   :with-slot-values
   :with-typed-slot-values
   :mac
   :nil!
   :*=
   :foo
   :for
   :time-elapsed
   :position-not
   :%100
   :dotimes-nested

   ;;;sequences
   :leng=1
   :leng=2
   :flatten
   :longer
   :make-list-meval
   :similarity
   :matrix->list
   :list->sequence
   :sequence->list

   ;;;arrays
   :make-typed-array
   :transpose
   :make-random-matrix
   :make-random-vector
   :!combine-float-matrices
   :!combine-float-vectors
   :vector=
   :matrix=
   :vector-from-matrix
   :!normalize-vector
   :!normalize-matrix
   :accum-array
   :copy-matrix

   ;;;strings
   :string-to-number
   :char->string

   ;;;others
   :file->vector
   :look-up
   :bin-search
   :lin-search
   :def-lin-search
   :turn-roulette
   :date
   :lisparray->pythonarray
   :make-hash-table-with-list))

