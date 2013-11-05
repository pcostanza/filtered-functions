(in-package :filtered-functions)

(defgeneric generic-function-filter-expression (gf)
  (:method ((gf generic-function)) (constantly t)))

(defclass simple-filtered-function (standard-generic-function)
  ((filter-expression :initarg :filter-expression
                      :reader generic-function-filter-expression))
  (:metaclass funcallable-standard-class))

(defmethod compute-applicable-methods-using-classes ((ff simple-filtered-function) classes)
  (declare (ignore classes))
  (values '() nil))

(defmethod compute-applicable-methods ((ff simple-filtered-function) required-args)
  (let* ((filter-expression (generic-function-filter-expression ff))
         (filter-functions (apply filter-expression required-args)))
    (cond ((consp filter-functions)
           (loop for arg in required-args
                 for filter-function = (pop filter-functions)
                 collect (if filter-function
                           (funcall filter-function arg)
                           arg)
                 into filtered-args
                 finally (return (call-next-method ff filtered-args))))
          ((null filter-functions) '())
          ((eq filter-functions 't) (call-next-method))
          (t (call-next-method ff (cons (funcall filter-functions (first required-args))
                                        (rest required-args)))))))

(defclass filtered-function (standard-generic-function)
  ((filter-groups :initform '() :reader %filter-groups))
  (:metaclass funcallable-standard-class))

(defun check-filters (ff filters)
  (when (assoc nil filters)
    (error "NIL is not a valid filter key in filtered function ~S." ff))
  (loop for (first . rest) on (mapcar #'first filters)
        when (member first rest)
        do (error "Duplicate filter specification ~S in filtered function ~S." first ff))
  (loop for (key . filter) in filters
        unless (or (symbolp filter) (functionp filter))
        do (error "~S is not a valid function designator for filter ~S in filtered function ~S."
                  filter key ff)))

(defmethod initialize-instance :after
  ((ff filtered-function) &key filters)
  (check-filters ff filters)
  (loop with initargs = `(,@(handler-case
                                (list :lambda-list
                                      (generic-function-lambda-list ff)
                                      :argument-precedence-order
                                      (generic-function-argument-precedence-order ff))
                              (error () '()))
                          :method-class ,(generic-function-method-class ff)
                          :method-combination ,(generic-function-method-combination ff))
        for (key . filter) in filters
        collect (cons key (apply #'make-instance
                                 'simple-filtered-function
                                 :filter-expression filter
                                 initargs)) into filter-groups
        finally
        (setf (slot-value ff 'filter-groups) filter-groups)))

(defmethod reinitialize-instance :after
  ((ff filtered-function) &rest initargs &key (filters '() filtersp))
  (if filtersp
    (loop initially (check-filters ff filters)
          with initargs = `(,@(handler-case
                                  (list :lambda-list
                                        (generic-function-lambda-list ff)
                                        :argument-precedence-order
                                        (generic-function-argument-precedence-order ff))
                                (error () '()))
                            :method-class ,(generic-function-method-class ff)
                            :method-combination ,(generic-function-method-combination ff))
          with old-filter-groups = (%filter-groups ff)
          for (key . filter) in filters
          for old-filter-group = (cdr (assoc key old-filter-groups))
          collect (cons key (if old-filter-group
                              (apply #'reinitialize-instance
                                     old-filter-group
                                     :filter-expression filter
                                     initargs)
                              (apply #'make-instance
                                     'simple-filtered-function
                                     :filter-expression filter
                                     initargs))) into filter-groups
          finally
          (setf (slot-value ff 'filter-groups) filter-groups))
    (loop for (nil . filter-group) in (%filter-groups ff)
          do (apply #'reinitialize-instance filter-group initargs))))

(defgeneric generic-function-filters (ff)
  (:method ((ff filtered-function))
   (loop for (key . filter-group) in (%filter-groups ff)
         collect (cons key (generic-function-filter-expression filter-group)))))

(defgeneric (setf generic-function-filters) (new-filters ff)
  (:argument-precedence-order ff new-filters)
  (:method ((new-filters list) (ff filtered-function))
   (reinitialize-instance ff :filters new-filters)
   new-filters))

(defvar *generic-functions*)

(defmethod compute-applicable-methods-using-classes ((ff filtered-function) classes)
  (declare (ignore classes))
  (if *generic-functions*
    (values '() nil)
    (call-next-method)))

(defmethod compute-applicable-methods ((ff filtered-function) args)
  (if *generic-functions*
    (append
     (loop for ff in *generic-functions*
           append (compute-applicable-methods ff args))
     (call-next-method))
    (call-next-method)))

#-ecl
(defmethod compute-discriminating-function ((ff filtered-function))
  (flet ((compute-discriminator ()
           (loop with gfs
                 for (nil . gf) in (%filter-groups ff)
                 when (generic-function-methods gf) do (push gf gfs)
                 finally
                 (return
                  (if (generic-function-methods ff)
                    (let ((original-discriminator (call-next-method)))
                      (lambda (&rest args)
                        (let ((*generic-functions* gfs))
                          (apply original-discriminator args))))
                    (cond ((null gfs)
                           (lambda (&rest args)
                             (apply #'no-applicable-method ff args)))
                          ((null (cdr gfs))
                           (compute-discriminating-function (car gfs)))
                          (t (let ((original-discriminator (call-next-method)))
                               (lambda (&rest args)
                                 (let ((*generic-functions* gfs))
                                   (apply original-discriminator args)))))))))))
    (if (eq (class-of ff) (find-class 'filtered-function))
      (lambda (&rest args)
        (let ((discriminator (compute-discriminator)))
          (set-funcallable-instance-function ff discriminator)
          (apply discriminator args)))
      (compute-discriminator))))

#+ecl
(defmethod compute-discriminating-function ((ff filtered-function))
  (let ((original-discriminator (call-next-method)))
    (flet ((compute-discriminator ()
             (loop with gfs
                   for (nil . gf) in (%filter-groups ff)
                   when (generic-function-methods gf) do (push gf gfs)
                   finally
                   (return
                    (if (generic-function-methods ff)
                      (lambda (&rest args)
                        (let ((*generic-functions* gfs))
                          (apply original-discriminator args)))
                      (cond ((null gfs)
                             (lambda (&rest args)
                               (apply #'no-applicable-method ff args)))
                            ((null (cdr gfs))
                             (compute-discriminating-function (car gfs)))
                            (t (lambda (&rest args)
                                 (let ((*generic-functions* gfs))
                                   (apply original-discriminator args))))))))))
      (if (eq (class-of ff) (find-class 'filtered-function))
        (lambda (&rest args)
          (let ((discriminator (compute-discriminator)))
            (set-funcallable-instance-function ff discriminator)
            (apply discriminator args)))
        (compute-discriminator)))))

(defgeneric method-filter (method)
  (:method ((method method)) nil))

(defclass filtered-method (standard-method)
  ((filter :initform nil :reader method-filter)))

(defmethod initialize-instance :after ((method filtered-method) &key qualifiers)
  (when (member (first qualifiers) '(:before :after :around))
    (pop qualifiers))
  (when (eq (first qualifiers) :filter)
    (pop qualifiers)
    (unless qualifiers
      (error "Filter qualifier is not followed by a filter designator in method ~S." method))
    (unless (first qualifiers)
      (error "NIL is not a valid filter designator in method ~S." method))
    (setf (slot-value method 'filter)
          (pop qualifiers)))
  (when qualifiers
    (error "Invalid qualifiers ~S for method ~S." qualifiers method)))

(defmethod add-method ((ff filtered-function) method)
  (let ((filter (method-filter method)))
    (if filter
      (let ((spec (assoc filter (%filter-groups ff))))
        (cond (spec (add-method (cdr spec) method))
              (t (cerror "Try again."
                         "Invalid filter ~S in method ~S for filtered function ~S." filter method ff)
                 (add-method ff method)))
        (reinitialize-instance ff))
      (call-next-method))))

(defmethod remove-method ((ff filtered-function) method)
  (let ((filter (method-filter method)))
    (if filter
      (let ((spec (assoc filter (%filter-groups ff))))
        (when spec
          (remove-method (cdr spec) method)
          (reinitialize-instance ff)))
      (call-next-method))))

(define-method-combination filtered ()
  ((methods * :required t))
  (:generic-function gf)
  (loop with after
        for method in methods
        for (qualifier) = (method-qualifiers method)
        if (eq qualifier :around) collect method into around
        else if (eq qualifier :before) collect method into before
        else if (eq qualifier :after) do (push method after)
        else collect method into primary
        finally (unless primary
                  (method-combination-error 
                   "No applicable primary method for generic function ~S." gf))
        (return 
         (flet ((call-methods (methods)
                  (loop for method in methods collect `(call-method ,method))))
           (let* ((inner-form (if (or before after (rest primary))
                                `(multiple-value-prog1
                                     (progn ,@(call-methods before)
                                       (call-method ,(first primary) ,(rest primary)))
                                   ,@(call-methods (reverse after)))
                                `(call-method ,(first primary))))
                  (outer-form (if around
                                `(call-method ,(first around)
                                              (,@(rest around)
                                               (make-method ,inner-form)))
                                inner-form)))
             outer-form)))))

(defmacro define-filtered-function (name (&rest lambda-list) &body options)
  `(progn
     (defgeneric ,name ,lambda-list
       ,@(unless (member :generic-function-class options :key #'first)
           '((:generic-function-class filtered-function)))
       ,@(unless (member :method-class options :key #'first)
           '((:method-class filtered-method)))
       ,@(unless (member :method-combination options :key #'first)
           '((:method-combination filtered)))
       ,@(remove :filters options :key #'first))
     (setf (generic-function-filters (fdefinition ',name))
           ,(let ((filters-option (rest (assoc :filters options)))
                  (required-lambda-list (loop for arg in lambda-list
                                              until (member arg lambda-list-keywords)
                                              collect arg)))
              `(list ,@(loop for (key . body) in filters-option
                             collect `(cons ',key (lambda ,required-lambda-list
                                                    (declare (ignorable ,@required-lambda-list))
                                                    ,@body))))))
     ',name))
