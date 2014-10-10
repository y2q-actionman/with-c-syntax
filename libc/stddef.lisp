(in-package #:with-c-syntax.stdlib)

(defun offsetof* (tag sym)
  (let ((spec-obj (with-c-syntax::find-wcs-struct-runtime-spec tag)))
    (cdr (assoc sym (with-c-syntax::runtime-spec-index-alist spec-obj)
                :test #'eq))))

(defmacro offsetof (struct-tag sym)
  `(offsetof* ',struct-tag ',sym))

(eval-when (:load-toplevel :execute)
  (define-preprocessor-symbol 'NULL 0)
  (define-predefined-typedef-and-aliases '|ptrdiff_t| 'fixnum
    '(|ptrdiff_t| |PTRDIFF_T|))
  (define-predefined-typedef-and-aliases '|size_t| 'fixnum
    '(|size_t| |SIZE_T|))
  (define-predefined-typedef-and-aliases '|wchar_t| 'fixnum
    '(|wchar_t| |WCHAR_T|))
  )
