(in-package #:with-c-syntax.stdlib)

;; single-float
(defconstant FLT_RADIX
  (float-radix 1f0))

(defconstant FLT_MANT_DIG
  (float-digits 1f0))

(defconstant FLT_EPSILON
  single-float-epsilon)

(defconstant FLT_DIG
  (mantissa-radix-change
     FLT_MANT_DIG FLT_RADIX 10))

(defconstant FLT_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-single-float)))

(defconstant FLT_MIN
  least-positive-normalized-single-float)

(defconstant FLT_MIN_10_EXP
  (ceiling (log least-positive-normalized-single-float 10)))
  
(defconstant FLT_MAX_EXP
  (nth-value 1 (decode-float most-positive-single-float)))

(defconstant FLT_MAX
  most-positive-single-float)

(defconstant FLT_MAX_10_EXP
  (floor (log most-positive-single-float 10)))

;; double-float
(defconstant DBL_RADIX                ; extension
  (float-radix 1d0))

(defconstant DBL_MANT_DIG
  (float-digits 1d0))

(defconstant DBL_EPSILON
  double-float-epsilon)

(defconstant DBL_DIG
  (mantissa-radix-change DBL_MANT_DIG DBL_RADIX 10))

(defconstant DBL_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-double-float)))

(defconstant DBL_MIN
  least-positive-normalized-double-float)

(defconstant DBL_MIN_10_EXP
  (ceiling (log least-positive-normalized-double-float 10)))
  
(defconstant DBL_MAX_EXP
  (nth-value 1 (decode-float most-positive-double-float)))

(defconstant DBL_MAX
  most-positive-double-float)

(defconstant DBL_MAX_10_EXP
  (floor (log most-positive-double-float 10)))

;; long-float
(defconstant LDBL_RADIX                ; extension
  (float-radix 1l0))

(defconstant LDBL_MANT_DIG
  (float-digits 1l0))

(defconstant LDBL_EPSILON
  long-float-epsilon)

(defconstant LDBL_DIG
  (mantissa-radix-change LDBL_MANT_DIG LDBL_RADIX 10))

(defconstant LDBL_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-long-float)))

(defconstant LDBL_MIN
  least-positive-normalized-long-float)

(defconstant LDBL_MIN_10_EXP
  (ceiling (log least-positive-normalized-long-float 10)))
  
(defconstant LDBL_MAX_EXP
  (nth-value 1 (decode-float most-positive-long-float)))

(defconstant LDBL_MAX
  most-positive-long-float)

(defconstant LDBL_MAX_10_EXP
  (floor (log most-positive-long-float 10)))

;; short-float; extension
(defconstant SFLT_RADIX
  (float-radix 1s0))

(defconstant SFLT_MANT_DIG
  (float-digits 1s0))

(defconstant SFLT_EPSILON
  short-float-epsilon)

(defconstant SFLT_DIG
  (mantissa-radix-change SFLT_MANT_DIG SFLT_RADIX 10))

(defconstant SFLT_MIN_EXP
  (nth-value 1 (decode-float least-positive-normalized-short-float)))

(defconstant SFLT_MIN
  least-positive-normalized-short-float)

(defconstant SFLT_MIN_10_EXP
  (ceiling (log least-positive-normalized-short-float 10)))
  
(defconstant SFLT_MAX_EXP
  (nth-value 1 (decode-float most-positive-short-float)))

(defconstant SFLT_MAX
  most-positive-short-float)

(defconstant SFLT_MAX_10_EXP
  (floor (log most-positive-short-float 10)))

;; XXX:
;; In ANSI CL, There is no way to accessing the Floating Point Mode.
;; I must use implementation-dependent features...
;; 
;; CMUCL: http://www.umiacs.umd.edu/~resnik/ling645_sp2002/cmu_manual/node22.html
(defconstant FLT_ROUNDS
  -1)

;; C99
(defconstant DECIMAL_DIG
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
(defconstant FLT_EVAL_METHOD
  -1)

(eval-when (:load-toplevel :execute)
;; single-float
(add-preprocessor-macro "FLT_RADIX" FLT_RADIX)
(add-preprocessor-macro "FLT_MANT_DIG" FLT_MANT_DIG)
(add-preprocessor-macro "FLT_EPSILON" FLT_EPSILON)
(add-preprocessor-macro "FLT_DIG" FLT_DIG)
(add-preprocessor-macro "FLT_MIN_EXP" FLT_MIN_EXP)
(add-preprocessor-macro "FLT_MIN" FLT_MIN)
(add-preprocessor-macro "FLT_MIN_10_EXP" FLT_MIN_10_EXP)
(add-preprocessor-macro "FLT_MAX_EXP" FLT_MAX_EXP)
(add-preprocessor-macro "FLT_MAX" FLT_MAX)
(add-preprocessor-macro "FLT_MAX_10_EXP" FLT_MAX_10_EXP)
;; double-float
(add-preprocessor-macro "DBL_RADIX" DBL_RADIX)
(add-preprocessor-macro "DBL_MANT_DIG" DBL_MANT_DIG)
(add-preprocessor-macro "DBL_EPSILON" DBL_EPSILON)
(add-preprocessor-macro "DBL_DIG" DBL_DIG)
(add-preprocessor-macro "DBL_MIN_EXP" DBL_MIN_EXP)
(add-preprocessor-macro "DBL_MIN" DBL_MIN)
(add-preprocessor-macro "DBL_MIN_10_EXP" DBL_MIN_10_EXP)
(add-preprocessor-macro "DBL_MAX_EXP" DBL_MAX_EXP)
(add-preprocessor-macro "DBL_MAX" DBL_MAX)
(add-preprocessor-macro "DBL_MAX_10_EXP" DBL_MAX_10_EXP)
;; long-float
(add-preprocessor-macro "LDBL_RADIX" LDBL_RADIX)
(add-preprocessor-macro "LDBL_MANT_DIG" LDBL_MANT_DIG)
(add-preprocessor-macro "LDBL_EPSILON" LDBL_EPSILON)
(add-preprocessor-macro "LDBL_DIG" LDBL_DIG)
(add-preprocessor-macro "LDBL_MIN_EXP" LDBL_MIN_EXP)
(add-preprocessor-macro "LDBL_MIN" LDBL_MIN)
(add-preprocessor-macro "LDBL_MIN_10_EXP" LDBL_MIN_10_EXP)
(add-preprocessor-macro "LDBL_MAX_EXP" LDBL_MAX_EXP)
(add-preprocessor-macro "LDBL_MAX" LDBL_MAX)
(add-preprocessor-macro "LDBL_MAX_10_EXP" LDBL_MAX_10_EXP)
;; short-float; extension
(add-preprocessor-macro "SFLT_RADIX" SFLT_RADIX)
(add-preprocessor-macro "SFLT_MANT_DIG" SFLT_MANT_DIG)
(add-preprocessor-macro "SFLT_EPSILON" SFLT_EPSILON)
(add-preprocessor-macro "SFLT_DIG" SFLT_DIG)
(add-preprocessor-macro "SFLT_MIN_EXP" SFLT_MIN_EXP)
(add-preprocessor-macro "SFLT_MIN" SFLT_MIN)
(add-preprocessor-macro "SFLT_MIN_10_EXP" SFLT_MIN_10_EXP)
(add-preprocessor-macro "SFLT_MAX_EXP" SFLT_MAX_EXP)
(add-preprocessor-macro "SFLT_MAX" SFLT_MAX)
(add-preprocessor-macro "SFLT_MAX_10_EXP" SFLT_MAX_10_EXP)
;; system
(add-preprocessor-macro "FLT_ROUNDS" FLT_ROUNDS)
(add-preprocessor-macro "DECIMAL_DIG" DECIMAL_DIG)
(add-preprocessor-macro "FLT_EVAL_METHOD" FLT_EVAL_METHOD)
)
