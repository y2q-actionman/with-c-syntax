(in-package :with-c-syntax)

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
