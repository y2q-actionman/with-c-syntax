(in-package #:with-c-syntax)

(defmacro eval-equal (val () &body body)
  (let ((exp (gensym)) (ret (gensym)))
    `(let ((,exp ,val)
	   (,ret (with-c-syntax () ,@body)))
       (assert (equal ,exp ,ret)
	       ()
	       "eval-equal error: expected ~S, returned ~S~% form ~S"
	       ,exp ,ret ',body))))

(defmacro assert-compile-error (() &body body)
  `(assert
    (nth-value
     1
     (ignore-errors
       (c-expression-tranform ',body
                              (or (getf (first *current-c-reader*) :case)
                                  (readtable-case *readtable*)))))))

(defmacro assert-runtime-error (() &body body)
  `(assert
    (nth-value
     1 
     (ignore-errors (with-c-syntax () ,@body)))))
