(in-package #:cl-user)

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax.core #:named-readtables
        #:alexandria
        #:1am
        #:float-features #:floating-point #:floating-point-contractions)
  (:documentation "with-c-syntax test package."))
