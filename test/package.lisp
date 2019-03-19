(in-package #:cl-user)

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax.core)
  (:import-from #:alexandria
		#:once-only)
  (:use #:named-readtables)
  (:export #:test-all)
  (:documentation "with-c-syntax test package."))
