(in-package #:cl-user)

(defpackage #:with-c-syntax.core
  (:use #:cl)
  (:import-from #:alexandria
                #:copy-hash-table	; 1. Hash Tables
		#:alist-hash-table
		#:define-constant	; 2. Data and Control Flow
		#:destructuring-ecase
		#:if-let
		#:when-let
		#:when-let*
		#:appendf		; 3. Conses
		#:nconcf
		#:nreversef
		#:lastcar
		#:removef		; 4. Sequences
		#:deletef
		#:length=
		#:once-only		; 6. Macro Writing
		#:with-gensyms
		#:maxf)			; 10. Numbers
  (:import-from #:yacc
        	#:define-parser
                #:parse-with-lexer
		#:yacc-parse-error)
  (:export
   ;; preprocessor.lisp
   #:find-preprocessor-macro
   #:add-preprocessor-macro
   #:remove-preprocessor-macro
   #:preprocessor
   ;; pseudo-pointer.lisp
   #:pseudo-pointer
   #:with-pseudo-pointer-scope
   #:invalidate-all-pseudo-pointers
   #:make-pseudo-pointer
   #:pseudo-pointer-dereference
   #:pseudo-pointer-invalidate
   #:pseudo-pointer-pointable-p
   ;; reader.lisp
   #:use-reader
   #:unuse-reader
   ;; struct.lisp
   #:struct
   #:make-struct
   #:struct-member
   ;; with-c-syntax.lisp
   #:find-typedef
   #:add-typedef
   #:remove-typedef
   #:find-struct-spec
   #:add-struct-spec
   #:remove-struct-spec
   #:get-varargs
   #:with-c-syntax)
  (:documentation
   "with-c-syntax core package."))
