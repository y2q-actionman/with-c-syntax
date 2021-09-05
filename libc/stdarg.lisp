(in-package #:with-c-syntax.libc-implementation)

(add-typedef '|va_list| 'list)

(defmacro |va_start| (ap last)
  `(setf ,ap (get-variadic-arguments ,last)))

;;; TODO: define 'va_arg' here or stdarg.h.

(defmacro |va_end| (ap)
  `(setf ,ap nil))

(defmacro |va_copy| (dest src)
  `(setf ,dest (copy-list ,src)))
