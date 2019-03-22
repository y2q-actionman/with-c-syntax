(in-package #:with-c-syntax.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pick-eql-name (symbol)
    (let* ((op-name (symbol-name symbol))
	   (eq-op-begin (1+ (position #\. op-name)))
	   (eq-op-end (position #\. op-name :start eq-op-begin)))
      (or (find-symbol (subseq op-name eq-op-begin eq-op-end))
	  (error "operator ~A does not contain expected equality function" symbol)))))

(defmacro define-is.*.wcs (operator)
  (let ((eql-op (pick-eql-name operator))
	(use-option (search "OPTION" (symbol-name operator))))
    `(defmacro ,operator (,@ (if use-option '(option) nil)
			  val &body body)
       `(is (,',eql-op
		,val
		(with-c-syntax (,@,(if use-option 'option nil))
		  ,@body))))))
			  
(define-is.*.wcs is.equal.wcs)
(define-is.*.wcs is.equalp.wcs)
(define-is.*.wcs is.equal.wcs.option)

(defmacro signals.macroexpand.wcs ((&optional (condition 'with-c-syntax-error)) &body body)
  `(signals ,condition
     (macroexpand '(with-c-syntax () ,@Body))))

(defmacro signals.wcs ((&optional (condition 'with-c-syntax-error)) &body body)
  `(signals ,condition
     (with-c-syntax () ,@Body)))

(defmacro muffle-unused-code-warning (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,@body)
  #-(or sbcl)			; To be supported..
  `(progn ,@body))
