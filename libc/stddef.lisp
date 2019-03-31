(in-package #:with-c-syntax.libc)

(define-preprocessor-symbol NULL 0)
(add-typedef '|ptrdiff_t| 'fixnum)
(add-typedef '|size_t| 'fixnum)
(add-typedef '|wchar_t| 'fixnum)
(define-preprocessor-symbol |offsetof| 'with-c-syntax.core::|__offsetof|)
