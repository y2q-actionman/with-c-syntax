(in-package #:with-c-syntax.libc)

;;; See +numeric-types-alist+

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun signed-byte-max (bits)
    (1- (expt 2 (1- bits))))

  (defun signed-byte-min (bits)
    (- (expt 2 (1- bits))))

  (defun unsigned-byte-max (bits)
    (1- (expt 2 bits))))

;; char: (signed-byte 8)
(define-preprocessor-symbol CHAR_BIT 8)
(define-preprocessor-symbol CHAR_MAX (signed-byte-max 8))
(define-preprocessor-symbol CHAR_MIN (signed-byte-min 8))

;; int: fixnum
(define-preprocessor-symbol INT_MAX most-positive-fixnum)
(define-preprocessor-symbol INT_MIN most-negative-fixnum)

;; short: (signed-byte 16)
(define-preprocessor-symbol SHRT_MAX (signed-byte-max 16))
(define-preprocessor-symbol SHRT_MIN (signed-byte-min 16))

;; long: (signed-byte 32)
(define-preprocessor-symbol LONG_MAX (signed-byte-max 32))
(define-preprocessor-symbol LONG_MIN (signed-byte-min 32))

;; long long: (signed-byte 64)
(define-preprocessor-symbol LLONG_MAX (signed-byte-max 64))
(define-preprocessor-symbol LLONG_MIN (signed-byte-min 64))

;; signed char: (signed-byte 8)
(define-preprocessor-symbol SCHAR_MAX (signed-byte-max 8))
(define-preprocessor-symbol SCHAR_MIN (signed-byte-min 8))

;; unsigned char: (unsigned-byte 8)
(define-preprocessor-symbol UCHAR_MAX (unsigned-byte-max 8))

;; unsigned int: see with-c-syntax.lisp
(define-preprocessor-symbol UINT_MAX (max most-positive-fixnum 65535))

;; unsigned short: (unsigned-byte 16)
(define-preprocessor-symbol USHRT_MAX (unsigned-byte-max 16))

;; unsigned long: (unsigned-byte 32)
(define-preprocessor-symbol ULONG_MAX (unsigned-byte-max 32))

;; unsigned long long: (unsigned-byte 64)
(define-preprocessor-symbol ULLONG_MAX (unsigned-byte-max 64))

;; FIXME:
(define-preprocessor-symbol MB_LEN_MAX
  (ceiling (1+ (floor (log char-code-limit 2))) CHAR_BIT))
