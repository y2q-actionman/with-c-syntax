(in-package #:cl-user)

(defpackage #:with-c-syntax.core
  (:use #:cl)
  (:import-from #:alexandria
                #:copy-hash-table	; 1. Hash Tables
		#:define-constant	; 2. Data and Control Flow
		#:destructuring-case
		#:if-let
		#:when-let
		#:when-let*
		#:appendf		; 3. Conses
		#:nreversef
		#:lastcar
		#:deletef		; 4. Sequences
		#:length=
		#:once-only		; 6. Macro Writing
		#:with-gensyms
		#:symbolicate		; 7. Symbols
		#:maxf)			; 10. Numbers
  (:import-from #:yacc
        	#:define-parser
                #:parse-with-lexer
		#:yacc-parse-error)
  (:export
   ;; condition.lisp
   #:with-c-syntax-error
   #:with-c-syntax-warning
   ;; preprocessor.lisp
   #:find-preprocessor-macro
   #:add-preprocessor-macro
   #:remove-preprocessor-macro
   #:preprocessor
   ;; pseudo-pointer.lisp
   #:pseudo-pointer
   #:with-pseudo-pointer-scope
   #:invalidate-all-pseudo-pointers
   #:pseudo-pointer-pointable-p
   #:make-pseudo-pointer
   #:pseudo-pointer-dereference
   #:pseudo-pointer-invalidate
   ;; reader.lisp
   #:use-reader
   #:unuse-reader
   ;; struct.lisp
   #:find-struct-spec
   #:add-struct-spec
   #:remove-struct-spec
   #:struct
   #:make-struct
   #:struct-member
   ;; typedef.lisp
   #:find-typedef
   #:add-typedef
   #:remove-typedef
   ;; with-c-syntax.lisp
   #:enum
   #:get-varargs
   #:with-c-syntax)
  (:documentation
   "with-c-syntax core package."))
