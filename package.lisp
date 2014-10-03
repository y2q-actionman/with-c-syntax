(in-package :cl-user)

(defpackage :with-c-syntax
  (:use :cl :yacc)
  (:import-from :alexandria
		:define-constant)
  (:export #:with-c-syntax
           #:c-expression-tranform))
