(in-package #:with-c-syntax.libc-implementation)

(add-typedef '|va_list| 'list)

(defmacro |va_start| (ap last)
  `(setf ,ap (get-variadic-arguments ,last)))

;;; FIXME: This is the only point using `define-preprocessor-function'. I want to remove this..
(define-preprocessor-function |va_arg| ((ap) (type))
  (declare (ignore type))		; TODO: FIXME: use this.
  `(pop ,ap))

(defmacro |va_end| (ap)
  `(setf ,ap nil))

(defmacro |va_copy| (dest src)
  `(setf ,dest (copy-list ,src)))
