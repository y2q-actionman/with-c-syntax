(in-package #:cl-user)

(defpackage #:with-c-syntax.libc
  (:use)
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
   ;; fenv
   #:|fexcept_t|
   #:FE_DIVBYZERO #:FE_INVALID #:FE_OVERFLOW #:FE_UNDERFLOW #:FE_ALL_EXCEPT
   #:|feclearexcept| #:|fegetexceptflag| #:|feraiseexcept| #:|fesetexceptflag| #:|fetestexcept|
   #:|fegetround|
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
   #:|float_t| #:|double_t|
   #:HUGE_VAL #:HUGE_VALF #:HUGE_VALL #:INFINITY #:NAN
   #:FP_INFINITE #:FP_NAN #:FP_NORMAL #:FP_SUBNORMAL #:FP_ZERO
   #:FP_ILOGB0 #:FP_ILOGBNAN
   #:MATH_ERRNO #:MATH_ERREXCEPT #:|math_errhandling|
   #:|fpclassify| #:|isfinite| #:|isinf| #:|isnan| #:|isnormal| #:|signbit|
   #:|acos| #:|asin| #:|atan| #:|atan2| #:|cos| #:|sin| #:|tan|
   #:|acosh| #:|asinh| #:|atanh| #:|cosh| #:|sinh| #:|tanh|
   #:|exp| #:|exp2| #:|expm1| #:FREXP* #:|ilogb| #:|ldexp| #:|log| #:|log10| #:|log1p| #:|log2| #:|logb| #:MODF* #:|scalbn| #:|scalbln|
   #:|cbrt| #:|fabs| #:|hypot| #:|pow| #:|sqrt|
   #:|ceil| #:|floor| #:|round| #:|lround| #:|llround| #:|trunc|
   #:|fmod| #:|remainder| #:REMQUO*
   #:|copysign| #:|nan| #:|nextafter| #:|nexttoward|
   #:|fdim| #:|fmax| #:|fmin|
   #:|isgreater| #:|isgreaterequal| #:|isless| #:|islessequal| #:|islessgreater| #:|isunordered|
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
