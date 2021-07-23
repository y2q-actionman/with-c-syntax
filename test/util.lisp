(in-package #:with-c-syntax.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (fboundp 'trivial-cltl2:compiler-let)
    (pushnew :with-c-syntax-test-use-compiler-let *features*)))

(defmacro define-is.*.wcs (name eql-operator-name &key (use-option nil))
  `(defmacro ,name (,@ (if use-option '(option) nil)
		       val &body body)
     `(is (,',eql-operator-name
	   ,val
	   (with-c-syntax (,@,(if use-option 'option nil))
	     ,@body)))))
			  
(define-is.*.wcs is.equal.wcs
  equal)
(define-is.*.wcs is.equalp.wcs
  equalp)
(define-is.*.wcs is.equal.wcs.option
  equal :use-option t)

(defmacro signals.macroexpand.wcs ((&optional (condition 'with-c-syntax-error)) &body body)
  `(signals ,condition
     (macroexpand '(with-c-syntax () ,@Body))))

(defmacro signals.wcs ((&optional (condition 'with-c-syntax-error)) &body body)
  `(signals ,condition
     (with-c-syntax () ,@Body)))

(defmacro is.wcs.reader (forms-in-wcs string)
  (with-gensyms (op options body)
    `(destructuring-bind (,op ,options &body ,body)
         (let ((*readtable* (find-readtable 'with-c-syntax:with-c-syntax-readtable)))
           (read-from-string ,string))
       (declare (ignore ,options))
       (assert (eq ,op 'with-c-syntax))
       (is (equal ,forms-in-wcs ,body)))))

#+ccl
(defun %signals.wcs.reader-ccl (expected fn)
  "KLUDGE: `1am:signals' catch CCL's internal condition,
  `ccl::parse-unknown-type'.  This code tries to ignore it."
  (flet ((handler (condition)
           (cond ((typep condition 'ccl::parse-unknown-type)
                  ;; decline
                  )
                 ((typep condition expected)
                  (1am::passed)
                  (return-from %signals.wcs.reader-ccl (values)))
                 (t (error "Expected to signal ~s, but got ~s:~%~a"
                           expected (type-of condition) condition)))))
    (handler-bind ((condition #'handler))
      (funcall fn)))
  (error "Expected to signal ~s, but got nothing." expected))

(defmacro signals.wcs.reader ((&optional (condition 'with-c-syntax-error)) string)
  (let ((body
          `(let ((*readtable* (find-readtable 'with-c-syntax:with-c-syntax-readtable)))
             (read-from-string ,string))))
    #-ccl
    `(signals ,condition ,body)
    #+ccl
    `(%signals.wcs.reader-ccl ',condition (lambda () ,body))))

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
