(in-package #:with-c-syntax.stdlib)

;; single-float
(defconstant FLT_RADIX
  #.(float-radix 1f0))

(defconstant FLT_MANT_DIG
  #.(float-digits 1f0))

(defconstant FLT_EPSILON
  single-float-epsilon)

(defconstant FLT_DIG
  #.(mantissa-radix-change
     FLT_MANT_DIG FLT_RADIX 10))

(defconstant FLT_MIN_EXP
  #.(nth-value 1 (decode-float least-positive-normalized-single-float)))

(defconstant FLT_MIN
  least-positive-normalized-single-float)

(defconstant FLT_MIN_10_EXP
  #.(ceiling (log least-positive-normalized-single-float 10)))
  
(defconstant FLT_MAX_EXP
  #.(nth-value 1 (decode-float most-positive-single-float)))

(defconstant FLT_MAX
  most-positive-single-float)

(defconstant FLT_MAX_10_EXP
  #.(floor (log most-positive-single-float 10)))

;; double-float
(defconstant DBL_RADIX                ; extension
  #.(float-radix 1d0))

(defconstant DBL_MANT_DIG
  #.(float-digits 1d0))

(defconstant DBL_EPSILON
  double-float-epsilon)

(defconstant DBL_DIG
  #.(mantissa-radix-change DBL_MANT_DIG DBL_RADIX 10))

(defconstant DBL_MIN_EXP
  #.(nth-value 1 (decode-float least-positive-normalized-double-float)))

(defconstant DBL_MIN
  least-positive-normalized-double-float)

(defconstant DBL_MIN_10_EXP
  #.(ceiling (log least-positive-normalized-double-float 10)))
  
(defconstant DBL_MAX_EXP
  #.(nth-value 1 (decode-float most-positive-double-float)))

(defconstant DBL_MAX
  most-positive-double-float)

(defconstant DBL_MAX_10_EXP
  #.(floor (log most-positive-double-float 10)))

;; long-float
(defconstant LDBL_RADIX                ; extension
  #.(float-radix 1l0))

(defconstant LDBL_MANT_DIG
  #.(float-digits 1l0))

(defconstant LDBL_EPSILON
  long-float-epsilon)

(defconstant LDBL_DIG
  #.(mantissa-radix-change LDBL_MANT_DIG LDBL_RADIX 10))

(defconstant LDBL_MIN_EXP
  #.(nth-value 1 (decode-float least-positive-normalized-long-float)))

(defconstant LDBL_MIN
  least-positive-normalized-long-float)

(defconstant LDBL_MIN_10_EXP
  #.(ceiling (log least-positive-normalized-long-float 10)))
  
(defconstant LDBL_MAX_EXP
  #.(nth-value 1 (decode-float most-positive-long-float)))

(defconstant LDBL_MAX
  most-positive-long-float)

(defconstant LDBL_MAX_10_EXP
  #.(floor (log most-positive-long-float 10)))

;; short-float; extension
(defconstant SFLT_RADIX
  #.(float-radix 1s0))

(defconstant SFLT_MANT_DIG
  #.(float-digits 1s0))

(defconstant SFLT_EPSILON
  short-float-epsilon)

(defconstant SFLT_DIG
  #.(mantissa-radix-change
     SFLT_MANT_DIG SFLT_RADIX 10))

(defconstant SFLT_MIN_EXP
  #.(nth-value 1 (decode-float least-positive-normalized-short-float)))

(defconstant SFLT_MIN
  least-positive-normalized-short-float)

(defconstant SFLT_MIN_10_EXP
  #.(ceiling (log least-positive-normalized-short-float 10)))
  
(defconstant SFLT_MAX_EXP
  #.(nth-value 1 (decode-float most-positive-short-float)))

(defconstant SFLT_MAX
  most-positive-short-float)

(defconstant SFLT_MAX_10_EXP
  #.(floor (log most-positive-short-float 10)))

;; XXX:
;; In ANSI CL, we have no way to accessing the Floating Point Mode.
;; We must use implementation-dependent features...
;; 
;; CMUCL: http://www.umiacs.umd.edu/~resnik/ling645_sp2002/cmu_manual/node22.html
(defconstant FLT_ROUNDS
  -1)

;; C99
(defconstant DECIMAL_DIG
  #.(max SFLT_DIG FLT_DIG DBL_DIG LDBL_DIG))

;; XXX:
;; 
;; The Hyperspec says 'Common Lisp functions assume that the accuracy
;; of arguments to them does not exceed their precision.' (in 12.1.4.2
;; Rule of Float Approximation).
;; This means the range and precision in numeric operations are same
;; as the type of its arguments, so 'FLT_EVAL_METHOD == 1'.
;; 
;; But, in "12.1.1.1 Associativity and Commutativity in Numeric
;; Operations", 
;;
;;   For functions that are mathematically associative (and possibly
;;   commutative), a conforming implementation may process the
;;   arguments in any manner consistent with associative (and possibly
;;   commutative) rearrangement.
;; 
;; This implies 'FLT_EVAL_METHOD == -1' when we use 3 or more args for
;; numeric operations.
;; 
;; In today's with-c-syntax, any numeric operations are restricted to
;; binary. But I think someone may implement *optimizations*
;; destroying this assumption -- in the far and far future.
(defconstant FLT_EVAL_METHOD
  -1)

(eval-when (:load-toplevel :execute)
;; single-float
(define-preprocessor-macro "FLT_RADIX" FLT_RADIX)
(define-preprocessor-macro "FLT_MANT_DIG" FLT_MANT_DIG)
(define-preprocessor-macro "FLT_EPSILON" FLT_EPSILON)
(define-preprocessor-macro "FLT_DIG" FLT_DIG)
(define-preprocessor-macro "FLT_MIN_EXP" FLT_MIN_EXP)
(define-preprocessor-macro "FLT_MIN" FLT_MIN)
(define-preprocessor-macro "FLT_MIN_10_EXP" FLT_MIN_10_EXP)
(define-preprocessor-macro "FLT_MAX_EXP" FLT_MAX_EXP)
(define-preprocessor-macro "FLT_MAX" FLT_MAX)
(define-preprocessor-macro "FLT_MAX_10_EXP" FLT_MAX_10_EXP)
;; double-float
(define-preprocessor-macro "DBL_RADIX" DBL_RADIX)
(define-preprocessor-macro "DBL_MANT_DIG" DBL_MANT_DIG)
(define-preprocessor-macro "DBL_EPSILON" DBL_EPSILON)
(define-preprocessor-macro "DBL_DIG" DBL_DIG)
(define-preprocessor-macro "DBL_MIN_EXP" DBL_MIN_EXP)
(define-preprocessor-macro "DBL_MIN" DBL_MIN)
(define-preprocessor-macro "DBL_MIN_10_EXP" DBL_MIN_10_EXP)
(define-preprocessor-macro "DBL_MAX_EXP" DBL_MAX_EXP)
(define-preprocessor-macro "DBL_MAX" DBL_MAX)
(define-preprocessor-macro "DBL_MAX_10_EXP" DBL_MAX_10_EXP)
;; long-float
(define-preprocessor-macro "LDBL_RADIX" LDBL_RADIX)
(define-preprocessor-macro "LDBL_MANT_DIG" LDBL_MANT_DIG)
(define-preprocessor-macro "LDBL_EPSILON" LDBL_EPSILON)
(define-preprocessor-macro "LDBL_DIG" LDBL_DIG)
(define-preprocessor-macro "LDBL_MIN_EXP" LDBL_MIN_EXP)
(define-preprocessor-macro "LDBL_MIN" LDBL_MIN)
(define-preprocessor-macro "LDBL_MIN_10_EXP" LDBL_MIN_10_EXP)
(define-preprocessor-macro "LDBL_MAX_EXP" LDBL_MAX_EXP)
(define-preprocessor-macro "LDBL_MAX" LDBL_MAX)
(define-preprocessor-macro "LDBL_MAX_10_EXP" LDBL_MAX_10_EXP)
;; short-float; extension
(define-preprocessor-macro "SFLT_RADIX" SFLT_RADIX)
(define-preprocessor-macro "SFLT_MANT_DIG" SFLT_MANT_DIG)
(define-preprocessor-macro "SFLT_EPSILON" SFLT_EPSILON)
(define-preprocessor-macro "SFLT_DIG" SFLT_DIG)
(define-preprocessor-macro "SFLT_MIN_EXP" SFLT_MIN_EXP)
(define-preprocessor-macro "SFLT_MIN" SFLT_MIN)
(define-preprocessor-macro "SFLT_MIN_10_EXP" SFLT_MIN_10_EXP)
(define-preprocessor-macro "SFLT_MAX_EXP" SFLT_MAX_EXP)
(define-preprocessor-macro "SFLT_MAX" SFLT_MAX)
(define-preprocessor-macro "SFLT_MAX_10_EXP" SFLT_MAX_10_EXP)
;; system
(define-preprocessor-macro "FLT_ROUNDS" FLT_ROUNDS)
(define-preprocessor-macro "DECIMAL_DIG" DECIMAL_DIG)
(define-preprocessor-macro "FLT_EVAL_METHOD" FLT_EVAL_METHOD)
)
