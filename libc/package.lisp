(in-package #:cl-user)

(defpackage #:with-c-syntax.libc
  (:use)
  (:import-from #:osicat-posix
                #:EDOM #:EILSEQ #:ERANGE)
  (:export
   ;; assert
   #:|assert| #:NDEBUG
   ;; ctype
   #:|isalnum| #:|isalpha| #:|islower| #:|isupper|
   #:|isdigit| #:|isxdigit| #:|iscntrl| #:|isgraph|
   #:|isspace| #:|isblank| #:|isprint| #:|ispunct|
   #:|tolower| #:|toupper|
   ;; errno
   #:|errno| #:EDOM #:EILSEQ #:ERANGE
   ;; float
   #:FLT_RADIX
   #:FLT_MANT_DIG #:FLT_EPSILON #:FLT_DIG
   #:FLT_MIN_EXP #:FLT_MIN #:FLT_MIN_10_EXP
   #:FLT_MAX_EXP #:FLT_MAX #:FLT_MAX_10_EXP
   #:DBL_MANT_DIG #:DBL_EPSILON #:DBL_DIG
   #:DBL_MIN_EXP #:DBL_MIN #:DBL_MIN_10_EXP
   #:DBL_MAX_EXP #:DBL_MAX #:DBL_MAX_10_EXP
   #:LDBL_MANT_DIG #:LDBL_EPSILON #:LDBL_DIG
   #:LDBL_MIN_EXP #:LDBL_MIN #:LDBL_MIN_10_EXP
   #:LDBL_MAX_EXP #:LDBL_MAX #:LDBL_MAX_10_EXP
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
   ;; math
   #:|fabs| #:|fmod| #:|remainder| #:REMQUO* #:|fmax| #:|fmin|
   #:|exp| #:|exp2| #:|expm1| #:|log| #:|log10| #:|log2| #:|log1p|
   #:|pow| #:|sqrt| #:|cbrt| #:|hypot|
   #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|atan2|
   #:|sinh| #:|cosh| #:|tanh| #:|asinh| #:|acosh| #:|atanh|
   #:|ceil| #:|floor| #:|trunc| #:|round|
   #:FREXP* #:|ldexp| #:MODF* #:|scalbn| #:|ilogb| #:|logb|
   #:FP_ILOGB0
   #:|copysign|
   #:HUGE_VAL
   #:HUGE_VALF #:HUGE_VALL #:INFINITY   ; C99
   #:|isnan| #:|isinf| #:|isfinite| #:|isnormal| #:|fpclassify| ; C99
   #:FP_NAN #:FP_INFINITE #:FP_ZERO #:FP_SUBNORMAL #:FP_NORMAL
   #:|signbit|                          ; C99
   #:|lerp|                             ; C++20
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
   #:|strlen| #:|strcmp| #:|strncmp| #:|strchr| #:|strrchr|
   #:|strspn| #:|strcspn| #:|strpbrk| #:|strstr| #:|strtok|
   #:|memchr| #:|memcmp| #:|memset| #:|memcpy| #:|memmove|)
  (:documentation
   "with-c-syntax libc package."))

(defpackage #:with-c-syntax.libc-implementation
  (:use #:cl #:with-c-syntax.core #:with-c-syntax.libc
        #:alexandria #:float-features)
  (:import-from #:floating-point-contractions
                #:exp-1 #:log1+ #:hypot)
  (:shadowing-import-from #:with-c-syntax.libc
                          #:NULL)
  (:documentation
   "with-c-syntax libc implemetation package."))
