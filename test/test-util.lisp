(in-package :with-c-syntax)

;; TODO: remove this!
(defun test (form)
  (format t "~&~S~%   ~S~%" form 
	  (c-expression-tranform form)))

(defmacro eval-equal (val () &body body)
  (let ((exp (gensym)) (ret (gensym)))
    `(let ((,exp ,val)
	   (,ret (with-c-syntax () ,@body)))
       (assert (equal ,exp ,ret)
	       ()
	       "eval-equal error: expected ~S, returned ~S~% form ~S"
	       ,exp ,ret ',body))))

;; used for pointer-variable, but not used now?
(defmacro eval-satifies (fun () &body body)
  (let ((ret (gensym)))
    `(let ((,ret (with-c-syntax () ,@body)))
       (assert (funcall ,fun ,ret)
	       ()
	       "eval-equal error: test-fun ~S, returned ~S~% form ~S"
	       ,fun ,ret ',body))))

(defmacro assert-compile-error (() &body body)
  `(multiple-value-bind (ret condition)
       (ignore-errors (c-expression-tranform ',body))
     (declare (ignore ret))
     (assert condition)))

(defmacro assert-runtime-error (() &body body)
  `(multiple-value-bind (ret condition)
       (ignore-errors (with-c-syntax ()
			,@body))
     (declare (ignore ret))
     (assert condition)))
