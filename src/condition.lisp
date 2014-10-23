(in-package #:with-c-syntax.core)

(define-condition preprocess-error (simple-error)
  ())

(define-condition pseudo-pointer-error (simple-error)
  ((pointer :initarg :pointer
	    :reader pseudo-pointer-error-pointer)
   (pointee :initarg :pointee
	   :reader pseudo-pointer-error-pointee)
   (offset :initarg :offset
	   :reader pseudo-pointer-error-offset)))

(define-condition pseudo-pointer-type-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Not a pointable type object: ~A."
	     (pseudo-pointer-error-pointee condition)))))

(define-condition pseudo-pointer-dangling-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Dangling pointer ~A (Object: ~A, Offset ~A)."
	     (pseudo-pointer-error-pointer condition)
	     (pseudo-pointer-error-pointee condition)
	     (pseudo-pointer-error-offset condition)))))

(define-condition pseudo-pointer-write-error (pseudo-pointer-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Not a writable pointer ~A."
	     (pseudo-pointer-error-pointee condition)))))
