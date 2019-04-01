(in-package #:with-c-syntax.libc)

(define-preprocessor-constant NULL 0)
(add-typedef '|ptrdiff_t| 'fixnum)
(add-typedef '|size_t| 'fixnum)
(add-typedef '|wchar_t| 'fixnum)
(define-preprocessor-constant |offsetof| 'with-c-syntax.core::|__offsetof|)
