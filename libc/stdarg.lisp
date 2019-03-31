(in-package #:with-c-syntax.libc)

(add-typedef '|va_list| 'list)

(define-preprocessor-function |va_start| ((ap) (last))
  (declare (ignore last))		; TODO: FIXME: use this.
  `(setf ,ap (get-variadic-arguments)))

(define-preprocessor-function |va_arg| ((ap) (type))
  (declare (ignore type))		; TODO: FIXME: use this.
  `(pop ,ap))

(define-preprocessor-function |va_end| ((ap))
  `(setf ,ap nil))

(define-preprocessor-function |va_copy| ((dest) (src))
  `(setf ,dest (copy-list ,src)))
