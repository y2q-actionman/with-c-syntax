(in-package :cl-user)

(defpackage :with-c-syntax
  (:use :cl :yacc)
  (:export #:with-c-syntax
           #:c-expression-tranform))
