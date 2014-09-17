(in-package :with-c-syntax)

;; TODO: remove this!
(defun test (form)
  (format t "~&~S~%   ~S~%" form 
	  (c-expression-tranform () form)))

(defmacro eval-equal (val (&rest bindings) &body body)
  (let ((exp (gensym)) (ret (gensym)))
    `(let ((,exp ,val)
	   (,ret (with-c-syntax (,@bindings) ,@body)))
       (assert (equal ,exp ,ret)
	       ()
	       "eval-equal error: expected ~S, returned ~S~% form ~S"
	       ,exp ,ret ',body))))

;; used for pointer-variable, but not used now?
(defmacro eval-satifies (fun (&rest bindings) &body body)
  (let ((ret (gensym)))
    `(let ((,ret (with-c-syntax (,@bindings) ,@body)))
       (assert (funcall ,fun ,ret)
	       ()
	       "eval-equal error: test-fun ~S, returned ~S~% form ~S"
	       ,fun ,ret ',body))))

(defmacro assert-compile-error ((&rest bindings) &body body)
  `(multiple-value-bind (ret condition)
       (ignore-errors (c-expression-tranform ',bindings
					     ',body))
     (declare (ignore ret))
     (assert condition)))