(in-package #:cl-user)

;;; with-c-syntax core.
(defpackage #:with-c-syntax
  (:use #:cl)
  (:import-from #:alexandria
                #:copy-hash-table	; 1. Hash Tables
		#:alist-hash-table
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
           ;; opened for stdlib
           #:define-preprocessor-symbol
           #:define-predefined-typedef
           #:get-varargs))

;;; C stdlibs.
(defpackage #:with-c-syntax.stdlib
  (:use #:cl)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:with-c-syntax
                #:define-preprocessor-symbol
                #:define-predefined-typedef
                #:get-varargs))

;;; for test.
(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax))
