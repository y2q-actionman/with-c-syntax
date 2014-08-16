(in-package :cl-user)

(asdf:defsystem :cl-with-c-syntax
  :serial t
  :depends-on (:alexandria :yacc)
  :components ((:file "package")
               (:file "pseudo-pointer")
	       (:file "yacc_text")))

(asdf:defsystem :cl-with-c-syntax.test
  :serial t
  :depends-on (:cl-with-c-syntax)
  :components ((:file "test-stmt")
	       (:file "test-decl")
	       (:file "test-expand")))