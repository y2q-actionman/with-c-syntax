(defsystem #:with-c-syntax-test
  :description "test for with-c-syntax."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :pathname #.(make-pathname :directory '(:relative "test"))
  :depends-on (#:with-c-syntax #:1am #:trivial-cltl2
                               #:floating-point)
  :components ((:file "package")
	       (:file "util" :depends-on ("package"))
               (:file "stmt" :depends-on ("util"))
	       (:file "decl" :depends-on ("util"))
               (:file "pointer" :depends-on ("util"))
	       (:file "trans" :depends-on ("util"))
	       (:file "wcs" :depends-on ("util"))
	       (:file "reader_level_0" :depends-on ("util"))
	       (:file "reader_level_1" :depends-on ("util"))
	       (:file "reader_level_2" :depends-on ("util"))
	       (:file "reader_parameters" :depends-on ("util"))
	       (:file "preprocessor" :depends-on ("util"))
	       (:file "examples" :depends-on ("util"))
	       (:file "libc__freestanding" :depends-on ("util"))
	       (:file "libc_assert" :depends-on ("util"))
	       (:file "libc_ctype" :depends-on ("util"))
	       (:file "libc_math" :depends-on ("util"))
	       (:file "libc_string" :depends-on ("util"))
               (:module "include"
                :components
                ((:module "test"
                  :components
                  ((:static-file "test-pp-6.10.3.5-example-3.h")
                   (:static-file "test-pp-6.10.3.5-example-4.h")
                   (:static-file "test-pp-6.10.3.5-example-7.h"))))))
  :perform (prepare-op :before (o c)
             (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
             (symbol-call '#:1am '#:run)))
