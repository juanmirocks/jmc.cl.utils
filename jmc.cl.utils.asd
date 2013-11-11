(in-package :cl-user)

(defpackage :net.ashrentum.utils.jmcejuela-system (:use :cl :asdf))
(in-package :net.ashrentum.utils.jmcejuela-system)

(defsystem jmc.cl.utils
    :name "jmc.cl.utils"
    :author "Juan Miguel Cejuela"
    :version "2013.11"
    :maintainer "Juan Miguel Cejuela"
    :licence "GPL 3"
    :description "Juan Miguel Cejuela's utilities for Common Lisp"
    :components
    ((:module
      "src"
      :components
      ((:file "packages")
       (:file "macros"
        :depends-on ("packages"))
       (:file "sequences"
        :depends-on ("macros" "packages"))
       (:file "arrays"
        :depends-on ("macros" "packages"))
       (:file "strings"
        :depends-on ("macros" "packages"))
       (:file "other"
        :depends-on ("macros" "packages"))))))
