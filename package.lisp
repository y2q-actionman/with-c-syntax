(in-package #:cl-user)

(defpackage #:with-c-syntax
  (:use #:cl)
  (:import-from #:with-c-syntax.core
                #:with-c-syntax
                #:use-reader
                #:unuse-reader)
  (:export #:with-c-syntax
           #:use-reader
           #:unuse-reader)
  (:documentation
   "with-c-syntax user package."))
