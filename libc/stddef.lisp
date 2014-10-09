(in-package #:with-c-syntax.stdlib.stddef)

(defconstant NULL 0)

(defun offsetof* (tag sym)
  (let ((spec-obj (with-c-syntax::find-wcs-struct-runtime-spec tag)))
    (cdr (assoc sym (with-c-syntax::runtime-spec-index-alist spec-obj)
                :test #'eq))))

(defmacro offsetof (struct-tag sym)
  `(offsetof* ',struct-tag ',sym))

(deftype |ptrdiff_t| ()
  'fixnum)

(deftype |size_t| ()
  'fixnum)

(deftype |wchar_t| ()
  'fixnum)

(eval-when (:load-toplevel :execute)
  (pushnew '(|ptrdiff_t| fixnum)
           *predefined-typedef-names*
           :test #'equal)
  (pushnew '(|size_t| fixnum)
           *predefined-typedef-names*
           :test #'equal)
  (pushnew '(|wchar_t| fixnum)
           *predefined-typedef-names*
           :test #'equal))
