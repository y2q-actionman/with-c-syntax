(in-package #:with-c-syntax.libc-implementation)

;;; I do not define NULL as a preprocessor macro. If do so, I
;;; destroy `CL:NULL' usage. I use `defconstant' and `defun' instead.

(defconstant NULL nil
  "Holding `CL:NIL' for acting NULL of C language.")

(defun NULL (object)
  "An alias for `CL:NULL'.
This is for convenience using 'NULL' as C constant and Lisp function
both when `intern' the libc package."
  (cl:null object))

(add-typedef '|ptrdiff_t| 'fixnum)
(add-typedef '|size_t| 'fixnum)
(add-typedef '|wchar_t| 'fixnum)
(define-preprocessor-constant |offsetof| '(with-c-syntax.core::|__offsetof|))
