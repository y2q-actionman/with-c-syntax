(in-package #:with-c-syntax.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (fboundp 'trivial-cltl2:compiler-let)
    (pushnew :with-c-syntax-test-use-compiler-let *features*)))

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
  "Muffles `sb-ext:code-deletion-note' of SBCL. For other
implementation, this is just `progn'"
  ;; I grepped Quicklisp codes with `sb-ext:code-deletion-note', but I
  ;; cannot found this kind of macros. So... is this useless?
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,@body)
  #-(or sbcl)
  `(progn ,@body))

(defmacro with-testing-wcs-bind ((&rest symbols) &body body)
  "Make SYMBOLS unbound outside of BODY."
  (assert (loop for sym in symbols
	     always (equal (symbol-package sym) *package*)))
  `(progn
     (when (or (some #'boundp ',symbols)
	       (some #'fboundp ',symbols)) 
       (warn "Some symbols are already bound. (bound ~{~A~^, ~}, fbound ~{~A~^, ~})"
	     (remove-if-not #'boundp ',symbols)
	     (remove-if-not #'fboundp ',symbols)))
     (unwind-protect (progn ,@body)
       (mapcar #'makunbound ',symbols)
       (mapcar #'fmakunbound ',symbols))))

(defmacro signals.wcs.reader ((&optional (condition 'with-c-syntax-error)) string)
  `(let ((*readtable* (copy-named-readtable 'with-c-syntax:with-c-syntax-readtable)))
     (signals ,condition
       (read-from-string ,string))))
