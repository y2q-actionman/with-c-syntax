(in-package #:with-c-syntax.stdlib.stdarg)

(deftype |va_list| ()
  'list)

;; The 'real' va_start is establised in the variadic func
(defmacro va_start (ap &optional last)
  (declare (ignore ap last))
  `(error "va_start() is called out of a variadic func"))

(defmacro va_arg (ap &optional type)
  (declare (ignore type))
  `(pop ,ap))

(defmacro va_end (ap)
  `(setf ,ap nil))

(defmacro va_copy (dest src)
  `(setf ,dest (copy-list ,src)))

(eval-when (:load-toplevel :execute)
  (pushnew '(|va_list| t)
           *predefined-typedef-names*
           :test #'equal))
