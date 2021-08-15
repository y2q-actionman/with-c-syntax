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
     (defun ,finder-function-name (string readtable-case)
       ,@(if docstring `(,docstring))
       (ecase readtable-case
         ((:upcase :invert)
          (if-let ((up-sym (find-symbol string ',upcased-package-name)))
            (symbol-value up-sym)))
         ((:downcase :preserve)
          (find-symbol string ',package-name))))))

(define-case-aware-find-symbol find-c-terminal
  #:with-c-syntax.syntax)

(define-case-aware-find-symbol find-preprocessor-directive
  #:with-c-syntax.preprocessor-directive)

(define-case-aware-find-symbol find-pp-operator-name
  #:with-c-syntax.preprocess-operator)

(define-case-aware-find-symbol find-pragma-name
  #:with-c-syntax.pragma-name)


(defmacro define-case-aware-token-p-function (function-name find-symbol-function symbol)
  `(defun ,function-name (token readtable-case)
     (and (symbolp token)
          (eq (,find-symbol-function (string token) readtable-case)
              ',symbol))))

(define-case-aware-token-p-function pp-pragma-directive-p
  find-preprocessor-directive
  with-c-syntax.preprocessor-directive:|pragma|)

(define-case-aware-token-p-function pp-defined-operator-p
  find-pp-operator-name
  with-c-syntax.preprocess-operator:|defined|)

(define-case-aware-token-p-function pp-pragma-operator-p
  find-pp-operator-name
  with-c-syntax.preprocess-operator:|_Pragma|)

(define-case-aware-token-p-function pp-stdc-pragma-p
  find-pragma-name
  with-c-syntax.pragma-name:|STDC|)

(define-case-aware-token-p-function pp-with-c-syntax-pragma-p
  find-pragma-name
  with-c-syntax.pragma-name:|WITH_C_SYNTAX|)

(define-case-aware-token-p-function pp-once-pragma-p
  find-pragma-name
  with-c-syntax.pragma-name:|once|)
