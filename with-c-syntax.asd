(defsystem #:with-c-syntax
  :description "with-c-syntax is a fun package which introduces the C language syntax into Common Lisp."
  :license "WTFPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria #:yacc #:named-readtables
                            #:cl-ppcre #:trivial-gray-streams #:asdf
                            ;; for libc implementation (TODO: split libc defsystem from core?)
                            #:osicat #:float-features #:floating-point-contractions)
  :serial t
  :components ((:file "package")
	       (:module "src"
		:serial nil	
                :components
                ((:file "package")
		 (:file "util" :depends-on ("package"))
		 (:file "condition" :depends-on ("package"))
                 (:file "physical-source" ; Translation Phase 1,2
                  :depends-on ("condition"))
                 (:file "reader"        ; Translation Phase 3
                  :depends-on ("physical-source"))
                 (:file "struct" :depends-on ("package"))
                 (:file "typedef" :depends-on ("package"))
                 (:file "pseudo-pointer" :depends-on ("util" "condition"))
                 (:file "lexer" :depends-on ("util" "reader" "typedef"))
                 (:file "compiler"      ; Translation Phase 7
		  :depends-on ("lexer" "struct" "typedef" "pseudo-pointer"))
                 (:file "preprocessor"  ; Translation Phase 4,(5),6
                  :depends-on ("util" "condition" "reader"
                               "lexer" "compiler")) ; Uses compiler for implementing '#if'
                 (:file "predefined-macro" :depends-on ("preprocessor"))
                 (:file "with-c-syntax" ; Entry Point
                  :depends-on ("preprocessor" "compiler"))))
               (:module "libc"
		:serial nil	
                :components
                ((:file "package")
		 (:file "util" :depends-on ("package"))
                 ;; alphabetical order
                 (:file "assert" :depends-on ("util"))
                 (:file "ctype" :depends-on ("util"))
                 (:file "errno" :depends-on ("util"))
                 (:file "fenv" :depends-on ("util"))
                 (:file "float" :depends-on ("util"))
                 (:file "iso646" :depends-on ("util"))
                 (:file "limits" :depends-on ("util"))
                 (:file "math" :depends-on ("util" "errno" "fenv" "float"))
                 (:file "stdarg" :depends-on ("util"))
                 (:file "stddef" :depends-on ("util"))
		 (:file "string" :depends-on ("util"))))
               (:module "include"
                :serial nil
                :components
                ((:static-file "iso646.h")
                 (:static-file "stdarg.h")
                 (:static-file "stddef.h"))))
  :in-order-to ((test-op (test-op #:with-c-syntax-test))))
