(in-package #:with-c-syntax.core)

(defmacro define-case-aware-find-symbol
    (finder-function-name package-name
     &key (upcased-package-name (format nil "~A.~A" (string package-name) '#:UPCASED))
       (docstring
        (format nil "Find a symbol in `~A' package having a same NAME. If not found, returns `nil'."
                package-name)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpackage ,upcased-package-name
       (:use)
       (:intern
        ,@(loop for sym being the external-symbol of package-name
                collect (string-upcase (symbol-name sym)))))
     ,@(loop for sym being the external-symbol of package-name
             for upcased = (string-upcase (symbol-name sym))
             collect `(setf (symbol-value (find-symbol ,upcased ',upcased-package-name))
                            ',sym))
     (defun ,finder-function-name (name readtable-case)
       ,@(if docstring `(,docstring))
       (ecase readtable-case
         ((:upcase :invert)
          (if-let ((up-sym (find-symbol name ',upcased-package-name)))
            (symbol-value up-sym)))
         ((:downcase :preserve)
          (find-symbol name ',package-name))))))

(define-case-aware-find-symbol find-c-terminal
  #:with-c-syntax.syntax)

(define-case-aware-find-symbol find-preprocessor-directive
  #:with-c-syntax.preprocessor-directive)

(define-case-aware-find-symbol find-pp-operator-name
  #:with-c-syntax.preprocess-operator)


#|
;;; Under implementation.

(defun pp-pragma-directive-p (token readtable-case)
  (and (symbolp token)
       (eq 'with-c-syntax.preprocessor-directive:|pragma|
           (find-preprocessor-directive token readtable-case))))

(defun pp-defined-operator-p (token readtable-case)
  (and (symbolp token)
       (eq 'with-c-syntax.preprocess-operator:|defined|
           (find-pp-operator-name token readtable-case))))

(defun pp-pragma-operator-p (token readtable-case)
  (and (symbolp token)
       (eq 'with-c-syntax.preprocess-operator:|_Pragma|
           (find-pp-operator-name token readtable-case))))
|#
