(in-package #:with-c-syntax.libc-implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mantissa-radix-change (mantissa from-radix to-radix)
    (+ (floor (* (1- mantissa)
		 (log from-radix to-radix)))
       (if (= from-radix to-radix) 1 0)))

  (let ((short-float-radix (float-radix 1s0))
        (single-float-radix (float-radix 1f0))
        (double-float-radix (float-radix 1d0))
        (long-float-radix (float-radix 1l0)))
    (unless (= short-float-radix single-float-radix double-float-radix long-float-radix)
      (warn
       "In this Lisp, the float radix is different for all types. (short ~A, single ~A, double ~A, long ~A"
       short-float-radix single-float-radix double-float-radix long-float-radix))))

(defconstant FLT_RADIX                ; I use double-float
  (float-radix 1d0))

;; single-float
(defconstant FLT_MANT_DIG
  (float-digits 1f0))

(defconstant FLT_EPSILON
  single-float-epsilon)

(defconstant FLT_DIG
  (mantissa-radix-change FLT_MANT_DIG (float-radix 1f0) 10))

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
(defconstant DBL_MANT_DIG
  (float-digits 1d0))

(defconstant DBL_EPSILON
  double-float-epsilon)

(defconstant DBL_DIG
  (mantissa-radix-change DBL_MANT_DIG (float-radix 1d0) 10))

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
(defconstant LDBL_MANT_DIG
  (float-digits 1l0))

(defconstant LDBL_EPSILON
  long-float-epsilon)

(defconstant LDBL_DIG
  (mantissa-radix-change LDBL_MANT_DIG (float-radix 1l0) 10))

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

;; CL has short-float, but I do not define them because they are not
;; required by C.

;; XXX:
;; In ANSI CL, There is no way to accessing the Floating Point Mode.
;; I must use implementation-dependent features...
;; 
;; CMUCL: http://www.umiacs.umd.edu/~resnik/ling645_sp2002/cmu_manual/node22.html
(defconstant FLT_ROUNDS
  -1)

;; C99
(defconstant DECIMAL_DIG
  (max FLT_DIG DBL_DIG LDBL_DIG))

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
