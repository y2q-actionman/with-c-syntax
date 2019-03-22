(in-package #:with-c-syntax.test)

(defmacro eval-equal (val (&rest options) &body body) ; obsoleted
  (let ((form `(with-c-syntax (,@options) ,@body)))
    (once-only (val (ret form))
      `(assert (equal ,val ,ret)
	       ()
	       "Expected ~S, but returned ~S~% form ~S."
	       ,val ,ret ',form))))

(defmacro define-is.*.wcs (operator)
  (let* ((op-name (symbol-name operator))
	 (eq-op-name (subseq op-name
			     (1+ (position #\. op-name))
			     (position #\. op-name :from-end t)))
	 (eq-op (find-symbol eq-op-name)))
    (assert eq-op () "operator ~A does not contain expected equality function" operator)
    `(defmacro ,operator (val &body body)
       `(is (,',eq-op ,val
		(with-c-syntax () ,@body))))))

(define-is.*.wcs is.equal.wcs)
(define-is.*.wcs is.equalp.wcs)
;;; TODO: is.equal.wcs.return

(defmacro assert-compile-error ((&rest options) &body body) ; obsoleted
  `(assert
    (nth-value
     1
     (ignore-errors
       (macroexpand
        '(with-c-syntax (,@options) ,@body))))))

(defmacro signals.macroexpand.wcs ((&optional (condition 'with-c-syntax-error)) &body body)
  `(signals ,condition
     (macroexpand '(with-c-syntax () ,@Body))))

(defmacro assert-runtime-error ((&rest options) &body body)
  `(assert
    (nth-value
     1 
     (ignore-errors (with-c-syntax (,@options) ,@body)))))

(defmacro muffle-unused-code-warning (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,@body)
  #-(or sbcl)			; To be supported..
  `(progn ,@body))
