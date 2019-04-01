(in-package #:with-c-syntax.libc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mantissa-radix-change (mantissa from-radix to-radix)
    (+ (floor (* (1- mantissa)
		 (log from-radix to-radix)))
       (if (= from-radix to-radix) 1 0))))

;; single-float
(define-preprocessor-constant FLT_RADIX
  (float-radix 1f0))

(define-preprocessor-constant FLT_MANT_DIG
  (float-digits 1f0))

(define-preprocessor-constant FLT_EPSILON
  single-float-epsilon)

(define-preprocessor-constant FLT_DIG
  (mantissa-radix-change
     FLT_MANT_DIG FLT_RADIX 10))

(define-preprocessor-constant FLT_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-single-float)))

(define-preprocessor-constant FLT_MIN
  least-positive-normalized-single-float)

(define-preprocessor-constant FLT_MIN_10_EXP
  (ceiling (log least-positive-normalized-single-float 10)))
  
(define-preprocessor-constant FLT_MAX_EXP
  (nth-value 1 (decode-float most-positive-single-float)))

(define-preprocessor-constant FLT_MAX
  most-positive-single-float)

(define-preprocessor-constant FLT_MAX_10_EXP
  (floor (log most-positive-single-float 10)))

;; double-float
(define-preprocessor-constant DBL_RADIX	; extension
  (float-radix 1d0))

(define-preprocessor-constant DBL_MANT_DIG
  (float-digits 1d0))

(define-preprocessor-constant DBL_EPSILON
  double-float-epsilon)

(define-preprocessor-constant DBL_DIG
  (mantissa-radix-change DBL_MANT_DIG DBL_RADIX 10))

(define-preprocessor-constant DBL_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-double-float)))

(define-preprocessor-constant DBL_MIN
  least-positive-normalized-double-float)

(define-preprocessor-constant DBL_MIN_10_EXP
  (ceiling (log least-positive-normalized-double-float 10)))
  
(define-preprocessor-constant DBL_MAX_EXP
  (nth-value 1 (decode-float most-positive-double-float)))

(define-preprocessor-constant DBL_MAX
  most-positive-double-float)

(define-preprocessor-constant DBL_MAX_10_EXP
  (floor (log most-positive-double-float 10)))

;; long-float
(define-preprocessor-constant LDBL_RADIX	; extension
  (float-radix 1l0))

(define-preprocessor-constant LDBL_MANT_DIG
  (float-digits 1l0))

(define-preprocessor-constant LDBL_EPSILON
  long-float-epsilon)

(define-preprocessor-constant LDBL_DIG
  (mantissa-radix-change LDBL_MANT_DIG LDBL_RADIX 10))

(define-preprocessor-constant LDBL_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-long-float)))

(define-preprocessor-constant LDBL_MIN
  least-positive-normalized-long-float)

(define-preprocessor-constant LDBL_MIN_10_EXP
  (ceiling (log least-positive-normalized-long-float 10)))
  
(define-preprocessor-constant LDBL_MAX_EXP
  (nth-value 1 (decode-float most-positive-long-float)))

(define-preprocessor-constant LDBL_MAX
  most-positive-long-float)

(define-preprocessor-constant LDBL_MAX_10_EXP
  (floor (log most-positive-long-float 10)))

;; short-float ; extension
(define-preprocessor-constant SFLT_RADIX
  (float-radix 1s0))

(define-preprocessor-constant SFLT_MANT_DIG
  (float-digits 1s0))

(define-preprocessor-constant SFLT_EPSILON
  short-float-epsilon)

(define-preprocessor-constant SFLT_DIG
  (mantissa-radix-change SFLT_MANT_DIG SFLT_RADIX 10))

(define-preprocessor-constant SFLT_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-short-float)))

(define-preprocessor-constant SFLT_MIN
  least-positive-normalized-short-float)

(define-preprocessor-constant SFLT_MIN_10_EXP
  (ceiling (log least-positive-normalized-short-float 10)))
  
(define-preprocessor-constant SFLT_MAX_EXP
  (nth-value 1 (decode-float most-positive-short-float)))

(define-preprocessor-constant SFLT_MAX
  most-positive-short-float)

(define-preprocessor-constant SFLT_MAX_10_EXP
  (floor (log most-positive-short-float 10)))

;; XXX:
;; In ANSI CL, There is no way to accessing the Floating Point Mode.
;; I must use implementation-dependent features...
;; 
;; CMUCL: http://www.umiacs.umd.edu/~resnik/ling645_sp2002/cmu_manual/node22.html
(define-preprocessor-constant FLT_ROUNDS
  -1)

;; C99
(define-preprocessor-constant DECIMAL_DIG
  (max SFLT_DIG FLT_DIG DBL_DIG LDBL_DIG))

;; XXX:
;; 
;; The Hyperspec says 'Common Lisp functions assume that the accuracy
;; of arguments to them does not exceed their precision.' (in 12.1.4.2
;; Rule of Float Approximation).
;; This means the range and precision in numeric operations are same
;; as the type of its arguments, so 'FLT_EVAL_METHOD == 0'.
;; 
;; But, in "12.1.1.1 Associativity and Commutativity in Numeric
;; Operations", 
;;
;;   For functions that are mathematically associative (and possibly
;;   commutative), a conforming implementation may process the
;;   arguments in any manner consistent with associative (and possibly
;;   commutative) rearrangement.
;; 
;; This implies 'FLT_EVAL_METHOD == -1' when I use 3 or more args for
;; numeric operations.
;; 
;; In today's with-c-syntax, any numeric operations are restricted to
;; binary. But I think someone may implement *optimizations*
;; destroying this assumption -- in the far and far future.
(define-preprocessor-constant FLT_EVAL_METHOD
  -1)
