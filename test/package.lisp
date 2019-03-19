(in-package #:cl-user)

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax.core
	#:named-readtables
	#:1am)
  (:import-from #:alexandria
		#:once-only)
  (:export #:test-all)
  (:documentation "with-c-syntax test package."))
