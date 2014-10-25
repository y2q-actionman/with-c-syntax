(in-package #:with-c-syntax.test)

(defmacro eval-equal (val (&rest options) &body body)
  (let ((form `(with-c-syntax (,@options) ,@body)))
    (once-only (val (ret form))
      `(assert (equal ,val ,ret)
	       ()
	       "Expected ~S, but returned ~S~% form ~S."
	       ,val ,ret ',form))))

(defmacro assert-compile-error ((&rest options) &body body)
  `(assert
    (nth-value
     1
     (ignore-errors
       (macroexpand
        '(with-c-syntax (,@options) ,@body))))))

(defmacro assert-runtime-error ((&rest options) &body body)
  `(assert
    (nth-value
     1 
     (ignore-errors (with-c-syntax (,@options) ,@body)))))

(defmacro muffle-unused-code-warning (&body body)
  `(locally
       (declare
        #+sbcl(sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,@body))
