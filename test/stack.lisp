(asdf:oos 'asdf:load-op :filtered-functions :force t)

(in-package :filtered-functions-user)

(defclass stack ()
  ((contents :initform (make-array 10) :reader stack-contents)
   (index :initform 0 :accessor stack-index)))

(defgeneric stack-state (stack)
  (:method ((stack stack))
   (cond ((<= (stack-index stack) 0) 'empty)
         ((>= (stack-index stack)
              (length (stack-contents stack))) 'full)
         (t 'normal))))

(define-filtered-function stack-push (stack value)
  (:filters (:state #'stack-state)))

(defmethod stack-push :filter :state ((stack (eql 'full)) value)
  (declare (ignore value))
  (error "Stack ~S is full." stack))

(defmethod stack-push (stack value)
  (prog1 (setf (svref (stack-contents stack)
                      (stack-index stack))
               value)
    (incf (stack-index stack))))

(define-filtered-function stack-pop (stack)
  (:filters (:state #'stack-state)))

(defmethod stack-pop :filter :state ((stack (eql 'empty)))
  (error "Stack ~S is empty." stack))

(defmethod stack-pop (stack)
  (svref (stack-contents stack)
         (decf (stack-index stack))))

(define-filtered-function stack-empty-p (stack)
  (:filters (:state  #'stack-state)))

(defmethod stack-empty-p :filter :state ((stack (eql 'empty))) t)

(defmethod stack-empty-p ((stack t)) nil)

(define-filtered-function stack-full-p (stack)
  (:filters (:state #'stack-state)))

(defmethod stack-full-p :filter :state ((stack (eql 'full))) t)

(defmethod stack-full-p ((stack t)) nil)

(let ((stack (make-instance 'stack)))
  (assert (stack-empty-p stack))
  (assert (not (stack-full-p stack)))
  (stack-push stack 1)
  (stack-push stack 2)
  (assert (not (stack-empty-p stack)))
  (assert (not (stack-full-p stack)))
  (assert (eql (stack-pop stack) 2))
  (assert (eql (stack-pop stack) 1))
  (assert (stack-empty-p stack))
  (assert (not (stack-full-p stack)))
  (assert (block test
            (handler-case (stack-pop stack)
              (error () (return-from test 't)))
            nil))
  (assert (block test
            (handler-case
                (progn (loop for i below 20 do (stack-push stack i))
                  (return-from test nil))
              (error ()
                (assert (not (stack-empty-p stack)))
                (assert (stack-full-p stack))
                (assert (equal
                         (loop until (stack-empty-p stack)
                               collect (stack-pop stack))
                         '(9 8 7 6 5 4 3 2 1 0)))
                t)))))

(print :done)
