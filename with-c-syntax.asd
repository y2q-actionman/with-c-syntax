(in-package #:cl-user)

(asdf:defsystem #:with-c-syntax
  :depends-on (#:alexandria #:yacc)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "util" :depends-on ("package"))
                 (:file "keywords" :depends-on ("package"))
                 (:file "struct" :depends-on ("package"))
                 (:file "pseudo-pointer" :depends-on ("util"))
                 (:file "preprocessor" :depends-on ("keywords"))
                 (:file "with-c-syntax"
		  :depends-on ("struct" "pseudo-pointer" "preprocessor"))
                 (:file "reader" :depends-on ("with-c-syntax"))))
               (:module "libc"
		:pathname #.(make-pathname :directory '(:relative "src" "libc"))
                :components
                ((:file "package")
		 (:file "util" :depends-on ("package"))
                 (:file "float" :depends-on ("util"))
                 (:file "iso646" :depends-on ("util"))
                 (:file "limits" :depends-on ("util"))
                 (:file "stdarg" :depends-on ("util"))
                 (:file "stddef" :depends-on ("util")))
                :depends-on ("src"))
	       (:file "package"
		:depends-on ("src" "libc"))))

(asdf:defsystem #:with-c-syntax.test
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:with-c-syntax)
  :components ((:file "package")
	       (:file "test-util" :depends-on ("package"))
               (:file "test-stmt" :depends-on ("test-util"))
	       (:file "test-decl" :depends-on ("test-util"))
               (:file "test-pointer" :depends-on ("test-util"))
	       (:file "test-trans" :depends-on ("test-util"))
	       (:file "test-wcs" :depends-on ("test-util"))
	       (:file "test-reader" :depends-on ("test-util"))
	       (:file "test-preprocessor" :depends-on ("test-util"))
	       (:file "test-examples" :depends-on ("test-util"))
	       (:file "test-all"
		:depends-on ("test-stmt" "test-decl" "test-pointer"
			     "test-trans" "test-wcs" "test-reader"
			     "test-preprocessor" "test-examples"))))
