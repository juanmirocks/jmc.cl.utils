;; Author: Juan Miguel Cejuela
;; Created: Sat Jul 12 19:54:16 2008 (CEST)
;; Last-Updated: 2011-08-11
;;     Update #: 6

(in-package :net.ashrentum.utils.jmcejuela)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-to-number (string)
  "Converts a string int a number"
  (declare (string string))
  (let ((number 0))
    (dotimes (i (length string) number)
      (setq number (* number 10))
      (incf number (digit-char-p (aref string i))))))

(defun char->string (char)
  "Converts a char into a string"
  (format nil "~a" char))

