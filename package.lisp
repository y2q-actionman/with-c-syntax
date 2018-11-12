(in-package #:cl-user)

(defpackage #:with-c-syntax
  (:use #:cl)
  (:import-from #:with-c-syntax.core
                #:with-c-syntax
		#:with-c-syntax-readtable
		#:*with-c-syntax-reader-level*
		#:*with-c-syntax-reader-case*)
  (:export #:with-c-syntax
           #:with-c-syntax-readtable
	   #:*with-c-syntax-reader-level*
	   #:*with-c-syntax-reader-case*)
  (:documentation
   "with-c-syntax user package."))
