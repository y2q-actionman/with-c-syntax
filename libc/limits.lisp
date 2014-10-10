(in-package #:with-c-syntax.stdlib)

;;; See +numeric-types-alist+

;; char: (signed-byte 8)
(defconstant CHAR_BIT 8)
(defconstant CHAR_MAX #.(signed-byte-max 8))
(defconstant CHAR_MIN #.(signed-byte-min 8))

;; int: fixnum
(defconstant INT_MAX most-positive-fixnum)
(defconstant INT_MIN most-negative-fixnum)

;; short: (signed-byte 16)
(defconstant SHRT_MAX #.(signed-byte-max 16))
(defconstant SHRT_MIN #.(signed-byte-min 16))

;; long: (signed-byte 32)
(defconstant LONG_MAX #.(signed-byte-max 32))
(defconstant LONG_MIN #.(signed-byte-min 32))

;; long long: (signed-byte 64)
(defconstant LLONG_MAX #.(signed-byte-max 64))
(defconstant LLONG_MIN #.(signed-byte-min 64))

;; signed char: (signed-byte 8)
(defconstant SCHAR_MAX #.(signed-byte-max 8))
(defconstant SCHAR_MIN #.(signed-byte-min 8))

;; unsigned char: (unsigned-byte 8)
(defconstant UCHAR_MAX #.(unsigned-byte-max 8))

;; unsigned int: see with-c-syntax.lisp
(defconstant UINT_MAX #.(max most-positive-fixnum 65535))

;; unsigned short: (unsigned-byte 16)
(defconstant USHRT_MAX #.(unsigned-byte-max 16))

;; unsigned long: (unsigned-byte 32)
(defconstant ULONG_MAX #.(unsigned-byte-max 32))

;; unsigned long long: (unsigned-byte 64)
(defconstant ULLONG_MAX #.(unsigned-byte-max 64))

;; FIXME:
(defconstant MB_LEN_MAX
  #.(ceiling (1+ (floor (log char-code-limit 2))) CHAR_BIT))


(eval-when (:load-toplevel :execute)
(define-preprocessor-symbol 'CHAR_BIT 'CHAR_BIT)
(define-preprocessor-symbol 'CHAR_MAX 'CHAR_MAX)
(define-preprocessor-symbol 'CHAR_MIN 'CHAR_MIN)
(define-preprocessor-symbol 'INT_MAX 'INT_MAX)
(define-preprocessor-symbol 'INT_MIN 'INT_MIN)
(define-preprocessor-symbol 'SHRT_MAX 'SHRT_MAX)
(define-preprocessor-symbol 'SHRT_MIN 'SHRT_MIN)
(define-preprocessor-symbol 'LONG_MAX 'LONG_MAX)
(define-preprocessor-symbol 'LONG_MIN 'LONG_MIN)
(define-preprocessor-symbol 'LLONG_MAX 'LLONG_MAX)
(define-preprocessor-symbol 'LLONG_MIN 'LLONG_MIN)
(define-preprocessor-symbol 'SCHAR_MAX 'SCHAR_MAX)
(define-preprocessor-symbol 'SCHAR_MIN 'SCHAR_MIN)
(define-preprocessor-symbol 'UCHAR_MAX 'UCHAR_MAX)
(define-preprocessor-symbol 'UINT_MAX 'UINT_MAX)
(define-preprocessor-symbol 'USHRT_MAX 'USHRT_MAX)
(define-preprocessor-symbol 'ULONG_MAX 'ULONG_MAX)
(define-preprocessor-symbol 'ULLONG_MAX 'ULLONG_MAX)
(define-preprocessor-symbol 'MB_LEN_MAX 'MB_LEN_MAX)
)
