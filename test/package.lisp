(in-package #:cl-user)

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax.core)
  (:import-from #:alexandria
		#:once-only)
  (:documentation
   "with-c-syntax test package. No symbols are exported."))
