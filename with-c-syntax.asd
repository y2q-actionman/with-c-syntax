(in-package #:cl-user)

(asdf:defsystem #:with-c-syntax
  :depends-on (#:alexandria #:yacc)
  :components ((:file "package")
	       (:file "util" :depends-on ("package"))
               (:file "wcs-struct" :depends-on ("package"))
               (:file "pseudo-pointer" :depends-on ("util"))
	       (:file "with-c-syntax" :depends-on ("wcs-struct"
						   "pseudo-pointer"))
               (:file "reader" :depends-on ("with-c-syntax"))))

(asdf:defsystem #:with-c-syntax.test
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:with-c-syntax)
  :components ((:file "test-util")
               (:file "test-stmt" :depends-on ("test-util"))
	       (:file "test-decl" :depends-on ("test-util"))
               (:file "test-pointer" :depends-on ("test-util"))
	       (:file "test-trans" :depends-on ("test-util"))
	       (:file "test-examples" :depends-on ("test-util"))
	       (:file "test-reader" :depends-on ("test-util"))
	       (:file "test-all" :depends-on ("test-stmt"
					      "test-decl"
					      "test-pointer"
					      "test-trans"
					      "test-examples"
					      "test-reader"))))
