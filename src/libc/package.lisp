(in-package #:cl-user)

(defpackage #:with-c-syntax.stdlib
  (:use #:cl)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:with-c-syntax.core
                #:define-preprocessor-macro
                #:add-typedef
                #:get-varargs)
  (:documentation
   "with-c-syntax libc package."))
