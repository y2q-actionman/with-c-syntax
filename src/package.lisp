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
   #:preprocessor
   #:define-preprocessor-macro
   ;; pseudo-pointer.lisp
   #:pseudo-pointer
   #:with-pseudo-pointer-scope
   ;; reader.lisp
   #:use-reader
   #:unuse-reader
   ;; struct.lisp
   #:struct
   #:make-struct
   #:struct-member
   ;; with-c-syntax.lisp
   #:add-typedef
   #:get-varargs
   #:with-c-syntax)
  (:documentation
   "with-c-syntax core package."))
