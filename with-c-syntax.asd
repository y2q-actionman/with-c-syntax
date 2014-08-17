(in-package :cl-user)

(asdf:defsystem :with-c-syntax
  :serial t
  :depends-on (:alexandria :yacc)
  :components ((:file "package")
               (:file "pseudo-pointer")
	       (:file "with-c-syntax")))
