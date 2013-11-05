(in-package :cl-user)

(defpackage #:filtered-functions
  (:use #:closer-common-lisp)
  (:export
   #:define-filtered-function
   #:filtered
   #:filtered-function
   #:filtered-method
   #:generic-function-filter-expression
   #:generic-function-filters
   #:method-filter
   #:simple-filtered-function))

(defpackage #:filtered-functions-user
  (:use #:closer-common-lisp #:filtered-functions))
