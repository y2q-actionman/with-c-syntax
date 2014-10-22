(in-package #:with-c-syntax.stdlib)

(eval-when (:load-toplevel :execute)
  (add-preprocessor-macro "NULL" 0)
  (add-predefined-typedef-and-aliases '|ptrdiff_t| 'fixnum)
  (add-predefined-typedef-and-aliases '|size_t| 'fixnum)
  (add-predefined-typedef-and-aliases '|wchar_t| 'fixnum)
  (add-preprocessor-macro-with-upcase "offsetof" 'with-c-syntax.core::|__offsetof|)
  )
