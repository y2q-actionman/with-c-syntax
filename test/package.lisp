(in-package #:cl-user)

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax.core #:named-readtables
        #:alexandria
        #:1am
        #:float-features #:floating-point #:floating-point-contractions)
  (:shadowing-import-from #:with-c-syntax.libc #:NULL)
  (:use #:with-c-syntax.libc)
  (:documentation "with-c-syntax test package."))
