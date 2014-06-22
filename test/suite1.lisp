(asdf:oos 'asdf:load-op :filtered-functions)

(in-package :filtered-functions-user)

(define-filtered-function fac (n)
  (:filters (:signum (when (integerp n) #'signum))))

(defmethod fac :filter :signum ((n (eql -1)))
  (error "Faculty is not defined for negative numbers."))

(defmethod fac :filter :signum ((n (eql 0))) 1)

(defmethod fac :filter :signum ((n (eql +1)))
  (* n (fac (1- n))))

(assert (eql (fac 3) 6))
(assert (eql (fac 7) 5040))
(assert (eql (fac 1) 1))
(assert (eql (fac 0) 1))
(assert (block test
          (handler-case (fac -3)
            (error () (return-from test t)))
          nil))

(define-filtered-function fac2 (n)
  (:filters (:signum  #'signum)))

(defmethod fac2 :filter :signum ((n (eql -1)))
  (error "Faculty is not defined for negative numbers."))

(defmethod fac2 :filter :signum ((n (eql 0))) 1)

(defmethod fac2 :filter :signum ((n (eql +1)))
  (* n (fac (1- n))))

(assert (eql (fac2 3) 6))
(assert (eql (fac2 7) 5040))
(assert (eql (fac2 1) 1))
(assert (eql (fac2 0) 1))
(assert (block test
          (handler-case (fac2 -3)
            (error () (return-from test t)))
          nil))

(defvar *environment* `((nil . nil) (t . t) (display . ,#'print) (add . ,#'+)))

(define-filtered-function evaluate (form)
  (:filters (:first (when (consp form) #'car))))

(defmethod evaluate (form) form)

(defmethod evaluate ((form symbol))
  (let ((cell (assoc form *environment*)))
    (if cell (cdr cell)
      (error "No binding found for symbol ~S." form))))

(defmethod evaluate :filter :first ((form (eql 'quote)))
  (second form))

(defmethod evaluate :filter :first ((form (eql 'setq)))
  (destructuring-bind (setq symbol value) form
    (declare (ignore setq))
    (let ((cell (assoc symbol *environment*))
          (value (evaluate value)))
      (if cell
        (setf (cdr cell) value)
        (setf *environment* (acons symbol value *environment*))))))

(defmethod evaluate :filter :first ((form (eql 'lambda)))
  (destructuring-bind (lambda args . body) form
    (declare (ignore lambda))
    (lambda (&rest vals)
      (let ((*environment* (pairlis args vals *environment*)))
        (loop for form in body
              for result = (evaluate form)
              finally (return result))))))

(defmethod evaluate :filter :first ((form (eql 'cond)))
  (destructuring-bind (cond . clauses) form
    (declare (ignore cond))
    (loop for (test consequence) in clauses
          when (evaluate test)
          return (evaluate consequence))))

(defmethod evaluate :filter :first ((form (eql 'let)))
  (destructuring-bind (let bindings . body) form
    (declare (ignore let))
    (evaluate `((lambda ,(mapcar #'first bindings) ,@body)
                ,@(mapcar #'second bindings)))))

(defmethod evaluate ((form cons))
  (let ((vals (mapcar #'evaluate form)))
    (apply (first vals) (rest vals))))

(assert (eql (evaluate '(let ((x 2) (y 3)) (display (add x y)))) 5))

(print :done)
