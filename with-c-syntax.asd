(in-package :cl-user)

(asdf:defsystem :with-c-syntax
  :serial t
  :depends-on (:alexandria :yacc)
  :components ((:file "package")
	       (:file "util")
               (:file "wcs-struct")
               (:file "pseudo-pointer")
	       (:file "with-c-syntax")))

(asdf:defsystem :with-c-syntax.test
  :serial t
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (:with-c-syntax)
  :components ((:file "test-util")
               (:file "test-stmt")
	       (:file "test-decl")
               (:file "test-pointer")
	       (:file "test-trans")
	       (:file "test-examples")
	       (:file "test-all")))
