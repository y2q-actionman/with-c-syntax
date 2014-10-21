(in-package #:with-c-syntax.stdlib)

(eval-when (:load-toplevel :execute)
  (define-preprocessor-macro "NULL" 0)
  (define-predefined-typedef-and-aliases '|ptrdiff_t| 'fixnum)
  (define-predefined-typedef-and-aliases '|size_t| 'fixnum)
  (define-predefined-typedef-and-aliases '|wchar_t| 'fixnum)
  (define-preprocessor-macro-with-upcase "offsetof" 'with-c-syntax.core::|__offsetof|)
  )
