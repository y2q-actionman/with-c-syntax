(in-package #:cl-user)

(defpackage #:with-c-syntax.libc
  (:use #:cl #:with-c-syntax.core)
  (:shadow #:null)
  (:export
   ;; assert
   #:|assert| #:NDEBUG
   ;; ctype
   #:|isalnum| #:|isalpha| #:|islower| #:|isupper|
   #:|isdigit| #:|isxdigit| #:|iscntrl| #:|isgraph|
   #:|isspace| #:|isblank| #:|isprint| #:|ispunct|
   #:|tolower| #:|toupper|
   ;; float
   #:FLT_RADIX #:FLT_MANT_DIG #:FLT_EPSILON #:FLT_DIG
   #:FLT_MIN_EXP #:FLT_MIN #:FLT_MIN_10_EXP
   #:FLT_MAX_EXP #:FLT_MAX #:FLT_MAX_10_EXP
   #:DBL_RADIX #:DBL_MANT_DIG #:DBL_EPSILON #:DBL_DIG
   #:DBL_MIN_EXP #:DBL_MIN #:DBL_MIN_10_EXP
   #:DBL_MAX_EXP #:DBL_MAX #:DBL_MAX_10_EXP
   #:LDBL_RADIX #:LDBL_MANT_DIG #:LDBL_EPSILON #:LDBL_DIG
   #:LDBL_MIN_EXP #:LDBL_MIN #:LDBL_MIN_10_EXP
   #:LDBL_MAX_EXP #:LDBL_MAX #:LDBL_MAX_10_EXP
   #:SFLT_RADIX #:SFLT_MANT_DIG #:SFLT_EPSILON #:SFLT_DIG
   #:SFLT_MIN_EXP #:SFLT_MIN #:SFLT_MIN_10_EXP
   #:SFLT_MAX_EXP #:SFLT_MAX #:SFLT_MAX_10_EXP
   #:FLT_ROUNDS
   #:DECIMAL_DIG
   #:FLT_EVAL_METHOD
   ;; iso646
   #:|and| #:|and_eq| #:|bitand| #:|bitor| #:|compl|
   #:|not| #:|not_eq| #:|or| #:|or_eq| #:|xor| #:|xor_eq|
   ;; limits
   #:CHAR_BIT
   #:CHAR_MAX #:CHAR_MIN #:INT_MAX #:INT_MIN
   #:SHRT_MAX #:SHRT_MIN #:LONG_MAX #:LONG_MIN
   #:LLONG_MAX #:LLONG_MIN #:SCHAR_MAX #:SCHAR_MIN
   #:UCHAR_MAX #:UINT_MAX #:USHRT_MAX #:ULONG_MAX #:ULLONG_MAX
   #:MB_LEN_MAX
   ;; stdarg
   #:|va_list|
   #:|va_start| #:|va_arg| #:|va_end|
   #:|va_copy|
   ;; stddef
   #:NULL
   #:|ptrdiff_t| #:|size_t| #:|wchar_t|
   #:|offsetof|
   ;; string
   #:|strcpy| #:|strncpy| #:|strcat| #:|strncat|
   #:|strlen| #:|strcmp| #:|strncmp|)
  (:documentation
   "with-c-syntax libc package."))

;;; At this point, the libc package was fixed. I build its cache here.
(with-c-syntax.core:build-libc-symbol-cache (find-package '#:with-c-syntax.libc))
