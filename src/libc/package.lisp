(in-package #:cl-user)

(defpackage #:with-c-syntax.stdlib
  (:use #:cl #:with-c-syntax.core)
  (:import-from #:alexandria
                #:length=)
  (:documentation
   "with-c-syntax libc package."))
