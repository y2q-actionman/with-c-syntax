(in-package #:cl-user)

(asdf:defsystem #:with-c-syntax
  :description "with-c-syntax is a fun package which introduces the C language syntax into Common Lisp."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :homepage "https://github.com/y2q-actionman/with-c-syntax"
  :depends-on (#:alexandria #:yacc)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "util" :depends-on ("package"))
		 (:file "condition" :depends-on ("package"))
                 (:file "keywords" :depends-on ("package"))
                 (:file "struct" :depends-on ("package"))
                 (:file "typedef" :depends-on ("package"))
                 (:file "pseudo-pointer" :depends-on ("util" "condition"))
                 (:file "preprocessor" :depends-on ("condition" "keywords"))
                 (:file "with-c-syntax"
		  :depends-on ("struct" "typedef" "pseudo-pointer" "preprocessor"))
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
                :depends-on ("src" "libc")))
  :in-order-to ((asdf:test-op (asdf:test-op #:with-c-syntax.test))))

(asdf:defsystem #:with-c-syntax.test
  :description "test for with-c-syntax."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :homepage "https://github.com/y2q-actionman/with-c-syntax"
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:with-c-syntax)
  :components ((:file "package")
	       (:file "util" :depends-on ("package"))
               (:file "stmt" :depends-on ("util"))
	       (:file "decl" :depends-on ("util"))
               (:file "pointer" :depends-on ("util"))
	       (:file "trans" :depends-on ("util"))
	       (:file "wcs" :depends-on ("util"))
	       (:file "reader" :depends-on ("util"))
	       (:file "preprocessor" :depends-on ("util"))
	       (:file "examples" :depends-on ("util"))
	       (:file "all"
		:depends-on ("stmt" "decl" "pointer"
			     "trans" "wcs" "reader"
			     "preprocessor" "examples")))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:with-c-syntax.test
                               '#:test-all)))
