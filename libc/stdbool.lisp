(in-package #:with-c-syntax.stdlib.stdbool)

(deftype |bool| ()
  'boolean)
    
(defconstant true t)

(defconstant false nil)

;; TODO: add this to *features*
(define-symbol-macro __bool_true_false_are_defined t)

(eval-when (:load-toplevel :execute)
  (pushnew '(|bool| t)
           *predefined-typedef-names*
           :test #'equal))
