(in-package #:with-c-syntax.core)

(define-condition with-c-syntax-error (simple-error)
  ()
  (:documentation
   "* Class Precedence List
with-c-syntax-error, simple-error, ...

* Description
The type ~with-c-syntax-error~ consists of all errors in with-c-syntax
system.
"))

(define-condition with-c-syntax-reader-error (with-c-syntax-error reader-error)
  ()
  (:documentation
   "Used in the with-c-syntax reader. Not for the installer of the with-c-syntax reader."))

(define-condition preprocess-error (with-c-syntax-error)
  ()
  (:documentation
   "Used in the preprocessor."))

(define-condition lexer-error (with-c-syntax-error)
  ((token :initarg :token
          :reader lexer-error-token))
  (:report
   (lambda (condition stream)
     (format stream "Bad token: ~A"
             (lexer-error-token condition))))
  (:documentation
   "Used in the lexer."))

(define-condition with-c-syntax-parse-error (with-c-syntax-error)
  ((yacc-error :initarg :yacc-error
               :reader with-c-syntax-parse-error-yacc-error))
  (:report
   (lambda (condition stream)
     (format stream "with-c-syntax parse error. yacc error is~%~A"
             (with-c-syntax-parse-error-yacc-error condition))))
  (:documentation
   "Used when an error occurred at the parser."))

(define-condition compile-error (with-c-syntax-error)
  ()
  (:documentation
   "Used at compiling with-c-syntax forms."))

(define-condition runtime-error (with-c-syntax-error)
  ()
  (:documentation
   "Used at evaluating with-c-syntax forms."))

(define-condition pseudo-pointer-error (runtime-error)
  ((pointer :initarg :pointer
	    :reader pseudo-pointer-error-pointer)
   (pointee :initarg :pointee
	   :reader pseudo-pointer-error-pointee)
   (offset :initarg :offset
	   :reader pseudo-pointer-error-offset))
  (:documentation
   "Used at using pseudo-pointers."))

(define-condition pseudo-pointer-type-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Not a pointable type object: ~A."
	     (pseudo-pointer-error-pointee condition))))
  (:documentation
   "Used when trying to make a pointer to a un-pointable object."))

(define-condition pseudo-pointer-dangling-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Dangling pointer ~A (Object: ~A, Offset ~A)."
	     (pseudo-pointer-error-pointer condition)
	     (pseudo-pointer-error-pointee condition)
	     (pseudo-pointer-error-offset condition))))
  (:documentation
   "Used when trying to use a dangling pointer."))

(define-condition pseudo-pointer-write-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The pointed object is not writable: ~A."
	     (pseudo-pointer-error-pointee condition))))
  (:documentation
   "Used when trying to write into a non-writable pointer."))

(define-condition with-c-syntax-warning (simple-warning)
  ()
  (:documentation
   "* Class Precedence List
with-c-syntax-warning, simple-warning, ...

* Description
The type ~with-c-syntax-warning~ consists of all warnings in the
with-c-syntax system.
"))
