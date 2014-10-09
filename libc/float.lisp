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
(defconstant |FLT_ROUNDS|
  -1)

;; single-float
(defconstant |FLT_RADIX|
  #.(float-radix 1f0))

(defconstant |FLT_MANT_DIG|
  #.(float-digits 1f0))

(defconstant |FLT_EPSILON|
  single-float-epsilon)

(defconstant |FLT_DIG|
  #.(mantissa-radix-change
     |FLT_MANT_DIG| |FLT_RADIX| 10))

(defconstant |FLT_MIN_EXP|
  #.(nth-value 1 (decode-float least-positive-normalized-single-float)))

(defconstant |FLT_MIN|
  least-positive-normalized-single-float)

(defconstant |FLT_MIN_10_EXP|
  #.(ceiling (log least-positive-normalized-single-float 10)))
  
(defconstant |FLT_MAX_EXP|
  #.(nth-value 1 (decode-float most-positive-single-float)))

(defconstant |FLT_MAX|
  most-positive-single-float)

(defconstant |FLT_MAX_10_EXP|
  #.(floor (log most-positive-single-float 10)))

;; double-float
(defconstant |DBL_RADIX|                ; extension
  #.(float-radix 1d0))

(defconstant |DBL_MANT_DIG|
  #.(float-digits 1d0))

(defconstant |DBL_EPSILON|
  double-float-epsilon)

(defconstant |DBL_DIG|
  #.(mantissa-radix-change |DBL_MANT_DIG| |DBL_RADIX| 10))

(defconstant |DBL_MIN_EXP|
  #.(nth-value 1 (decode-float least-positive-normalized-double-float)))

(defconstant |DBL_MIN|
  least-positive-normalized-double-float)

(defconstant |DBL_MIN_10_EXP|
  #.(ceiling (log least-positive-normalized-double-float 10)))
  
(defconstant |DBL_MAX_EXP|
  #.(nth-value 1 (decode-float most-positive-double-float)))

(defconstant |DBL_MAX|
  most-positive-double-float)

(defconstant |DBL_MAX_10_EXP|
  #.(floor (log most-positive-double-float 10)))

;; long-float
(defconstant |LDBL_RADIX|                ; extension
  #.(float-radix 1l0))

(defconstant |LDBL_MANT_DIG|
  #.(float-digits 1l0))

(defconstant |LDBL_EPSILON|
  long-float-epsilon)

(defconstant |LDBL_DIG|
  #.(mantissa-radix-change |LDBL_MANT_DIG| |LDBL_RADIX| 10))

(defconstant |LDBL_MIN_EXP|
  #.(nth-value 1 (decode-float least-positive-normalized-long-float)))

(defconstant |LDBL_MIN|
  least-positive-normalized-long-float)

(defconstant |LDBL_MIN_10_EXP|
  #.(ceiling (log least-positive-normalized-long-float 10)))
  
(defconstant |LDBL_MAX_EXP|
  #.(nth-value 1 (decode-float most-positive-long-float)))

(defconstant |LDBL_MAX|
  most-positive-long-float)

(defconstant |LDBL_MAX_10_EXP|
  #.(floor (log most-positive-long-float 10)))

;; short-float; extension
(defconstant |SFLT_RADIX|
  #.(float-radix 1s0))

(defconstant |SFLT_MANT_DIG|
  #.(float-digits 1s0))

(defconstant |SFLT_EPSILON|
  short-float-epsilon)

(defconstant |SFLT_DIG|
  #.(mantissa-radix-change
     |SFLT_MANT_DIG| |SFLT_RADIX| 10))

(defconstant |SFLT_MIN_EXP|
  #.(nth-value 1 (decode-float least-positive-normalized-short-float)))

(defconstant |SFLT_MIN|
  least-positive-normalized-short-float)

(defconstant |SFLT_MIN_10_EXP|
  #.(ceiling (log least-positive-normalized-short-float 10)))
  
(defconstant |SFLT_MAX_EXP|
  #.(nth-value 1 (decode-float most-positive-short-float)))

(defconstant |SFLT_MAX|
  most-positive-short-float)

(defconstant |SFLT_MAX_10_EXP|
  #.(floor (log most-positive-short-float 10)))
