(in-package :cl-user)

(asdf:defsystem :with-c-syntax.test
  :serial t
  :depends-on (:with-c-syntax)
  :components ((:file "test-util")
               (:file "test-stmt")
	       (:file "test-decl")
	       (:file "test-expand")))
