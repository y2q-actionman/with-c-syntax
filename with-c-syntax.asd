(in-package :cl-user)

(asdf:defsystem :with-c-syntax
  :serial t
  :depends-on (:alexandria :yacc)
  :components ((:file "package")
	       (:file "util")
               (:file "wcs-struct")
               (:file "pseudo-pointer")
	       (:file "with-c-syntax")))
