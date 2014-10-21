(in-package #:with-c-syntax.stdlib)

;;; See +numeric-types-alist+

;; char: (signed-byte 8)
(defconstant CHAR_BIT 8)
(defconstant CHAR_MAX (signed-byte-max 8))
(defconstant CHAR_MIN (signed-byte-min 8))

;; int: fixnum
(defconstant INT_MAX most-positive-fixnum)
(defconstant INT_MIN most-negative-fixnum)

;; short: (signed-byte 16)
(defconstant SHRT_MAX (signed-byte-max 16))
(defconstant SHRT_MIN (signed-byte-min 16))

;; long: (signed-byte 32)
(defconstant LONG_MAX (signed-byte-max 32))
(defconstant LONG_MIN (signed-byte-min 32))

;; long long: (signed-byte 64)
(defconstant LLONG_MAX (signed-byte-max 64))
(defconstant LLONG_MIN (signed-byte-min 64))

;; signed char: (signed-byte 8)
(defconstant SCHAR_MAX (signed-byte-max 8))
(defconstant SCHAR_MIN (signed-byte-min 8))

;; unsigned char: (unsigned-byte 8)
(defconstant UCHAR_MAX (unsigned-byte-max 8))

;; unsigned int: see with-c-syntax.lisp
(defconstant UINT_MAX (max most-positive-fixnum 65535))

;; unsigned short: (unsigned-byte 16)
(defconstant USHRT_MAX (unsigned-byte-max 16))

;; unsigned long: (unsigned-byte 32)
(defconstant ULONG_MAX (unsigned-byte-max 32))

;; unsigned long long: (unsigned-byte 64)
(defconstant ULLONG_MAX (unsigned-byte-max 64))

;; FIXME:
(defconstant MB_LEN_MAX
  (ceiling (1+ (floor (log char-code-limit 2))) CHAR_BIT))


(eval-when (:load-toplevel :execute)
(define-preprocessor-macro "CHAR_BIT" CHAR_BIT)
(define-preprocessor-macro "CHAR_MAX" CHAR_MAX)
(define-preprocessor-macro "CHAR_MIN" CHAR_MIN)
(define-preprocessor-macro "INT_MAX" INT_MAX)
(define-preprocessor-macro "INT_MIN" INT_MIN)
(define-preprocessor-macro "SHRT_MAX" SHRT_MAX)
(define-preprocessor-macro "SHRT_MIN" SHRT_MIN)
(define-preprocessor-macro "LONG_MAX" LONG_MAX)
(define-preprocessor-macro "LONG_MIN" LONG_MIN)
(define-preprocessor-macro "LLONG_MAX" LLONG_MAX)
(define-preprocessor-macro "LLONG_MIN" LLONG_MIN)
(define-preprocessor-macro "SCHAR_MAX" SCHAR_MAX)
(define-preprocessor-macro "SCHAR_MIN" SCHAR_MIN)
(define-preprocessor-macro "UCHAR_MAX" UCHAR_MAX)
(define-preprocessor-macro "UINT_MAX" UINT_MAX)
(define-preprocessor-macro "USHRT_MAX" USHRT_MAX)
(define-preprocessor-macro "ULONG_MAX" ULONG_MAX)
(define-preprocessor-macro "ULLONG_MAX" ULLONG_MAX)
(define-preprocessor-macro "MB_LEN_MAX" MB_LEN_MAX)
)
