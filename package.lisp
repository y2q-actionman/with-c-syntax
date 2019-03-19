(in-package #:cl-user)

(defpackage #:with-c-syntax
  (:export #:with-c-syntax
           #:with-c-syntax-readtable
	   #:*with-c-syntax-reader-level*
	   #:*with-c-syntax-reader-case*)
  (:documentation
   "The with-c-syntax package, holding public APIs."))
