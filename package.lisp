(in-package #:cl-user)

;;; with-c-syntax core.
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
           ;; opened for stdlib
           #:*predefined-typedef-names*
           #:va_start
           ))

;;; C stdlibs.
(defpackage #:with-c-syntax.stdlib.float
  (:use #:cl)
  (:export #:FLT_ROUNDS
           #:FLT_RADIX
           #:FLT_MANT_DIG
           #:FLT_EPSILON
           #:FLT_DIG
           #:FLT_MIN_EXP
           #:FLT_MIN
           #:FLT_MIN_10_EXP
           #:FLT_MAX_EXP
           #:FLT_MAX
           #:FLT_MAX_10_EXP
           #:DBL_RADIX                ; extension
           #:DBL_MANT_DIG
           #:DBL_EPSILON
           #:DBL_DIG
           #:DBL_MIN_EXP
           #:DBL_MIN
           #:DBL_MIN_10_EXP
           #:DBL_MAX_EXP
           #:DBL_MAX
           #:DBL_MAX_10_EXP
           #:LDBL_RADIX                ; extension
           #:LDBL_MANT_DIG
           #:LDBL_EPSILON
           #:LDBL_DIG
           #:LDBL_MIN_EXP
           #:LDBL_MIN
           #:LDBL_MIN_10_EXP
           #:LDBL_MAX_EXP
           #:LDBL_MAX
           #:LDBL_MAX_10_EXP
           ;; short-float; extension
           #:SFLT_RADIX
           #:SFLT_MANT_DIG
           #:SFLT_EPSILON
           #:SFLT_DIG
           #:SFLT_MIN_EXP
           #:SFLT_MIN
           #:SFLT_MIN_10_EXP
           #:SFLT_MAX_EXP
           #:SFLT_MAX
           #:SFLT_MAX_10_EXP
	   ;; C99
	   #:DECIMAL_DIG
	   #:FLT_EVAL_METHOD))

(defpackage #:with-c-syntax.stdlib.iso646
  (:use #:cl)
  (:export #:|and|
           #:|and_eq|
           #:|bitand|
           #:|bitor|
           #:|compl|
           #:|not|
           #:|not_eq|
           #:|or|
           #:|or_eq|
           #:|xor|
           #:|xor_eq|))

(defpackage #:with-c-syntax.stdlib.limits
  (:use #:cl)
  (:export #:CHAR_BIT
           #:CHAR_MAX
           #:CHAR_MIN
           #:INT_MAX
           #:INT_MIN
           #:SHRT_MAX
           #:SHRT_MIN
           #:LONG_MAX
           #:LONG_MIN
           #:LLONG_MAX
           #:LLONG_MIN
           #:SCHAR_MAX
           #:SCHAR_MIN
           #:UCHAR_MAX
           #:UINT_MAX
           #:USHRT_MAX
           #:ULONG_MAX
           #:ULLONG_MAX
           #:MB_LEN_MAX))

(defpackage #:with-c-syntax.stdlib.stdarg
  (:use #:cl)
  (:import-from #:with-c-syntax
                #:*predefined-typedef-names*
                #:va_start)
  (:export #:|va_list|
           #:va_start
           #:va_arg
           #:va_end
           #:va_copy))

(defpackage #:with-c-syntax.stdlib.stdbool
  (:use #:cl)
  (:import-from #:with-c-syntax
                #:*predefined-typedef-names*)
  (:export #:|bool|
           #:true
           #:false
           #:__bool_true_false_are_defined))

(defpackage #:with-c-syntax.stdlib.stddef
  (:use #:cl)
  (:import-from #:with-c-syntax
                #:*predefined-typedef-names*
                ;; TODO: add funcs for offsetof()
                )
  (:export #:NULL
           #:offsetof
           #:|ptrdiff_t|
           #:|size_t|
           #:|wchar_t|))

;;; for test.
(defpackage #:with-c-syntax.test
  (:use #:cl #:with-c-syntax
        #:with-c-syntax.stdlib.stdarg))
