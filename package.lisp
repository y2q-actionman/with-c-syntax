(in-package #:cl-user)

(defpackage #:with-c-syntax
  (:use #:cl)
  (:import-from #:alexandria
		#:alist-hash-table	; 1. Hash Tables
		#:define-constant	; 2. Data and Control Flow
		#:destructuring-ecase
		#:if-let
		#:when-let
		#:appendf		; 3. Conses
		#:nconcf
		#:lastcar
		#:removef		; 4. Sequences
		#:deletef
		#:length=
		#:once-only		; 6. Macro Writing
		#:with-gensyms
		#:maxf)			; 10. Numbers
  (:import-from #:yacc
        	#:define-parser
                #:parse-with-lexer)
  (:export #:with-c-syntax
	   #:use-reader
	   #:unuse-reader
           ;; TODO:
           #:pseudo-pointer
           ;; TODO: move to stdarg
           #:va_arg
           #:va_end
           #:va_start
           #:va_copy))

(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax))
