(in-package #:with-c-syntax.stdlib.float)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun mantissa-radix-change (mantissa from-radix to-radix)
  (+ (floor (* (1- mantissa)
               (log from-radix to-radix)))
     (if (= from-radix to-radix) 1 0))))
  
;; XXX:
;; In ANSI CL, we have no way to accessing the Floating Point Mode.
;; We must use implementation-dependent features...
;; 
;; CMUCL: http://www.umiacs.umd.edu/~resnik/ling645_sp2002/cmu_manual/node22.html
(defconstant FLT_ROUNDS
  -1)

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
