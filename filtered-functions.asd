#+(or abcl cmu mcl scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Filtered functions are currently not supported in this Common Lisp implementation."))

(asdf:defsystem #:filtered-functions
  :name "Filtered Functions"
  :description "Filtered functions provide an extension of generic function invocation that add a simple preprocessing step before the actual method dispatch is performed and thus enable the use of arbitrary predicates for selecting and applying methods."
  :author "Pascal Costanza"
  :version "0.2.0"
  :licence "MIT-style license"
  :depends-on (#:closer-mop)
  :components ((:file "filtered-functions-packages")
               (:file "filtered-functions"
                :depends-on ("filtered-functions-packages"))))
