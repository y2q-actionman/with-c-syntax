(in-package #:with-c-syntax.libc-implementation)

;;; I define only type-generic functions like <tgmath.h>,
;;; The numeric functions of Common Lisp are already so.

(defun |fabs| (x)
  (abs x))                              ; no error

(defun |fmod| (x y)
  (nth-value 1 (ftruncate x y)))        ; may raise EDOM, FE_INVALID

(defun |remainder| (x y)                ; C99
  (nth-value 1 (fround x y)))           ; may raise EDOM, FE_INVALID

(defun remquo* (x y)                    ; C99
  ;; TODO: support pointer passing..
  (multiple-value-bind (quotient remainder)
      (round x y)
    (values remainder quotient)))       ; may raise EDOM, FE_INVALID

;;; TODO: real 'remquo'

;;; TODO: 'fma', FP_FAST_FMA

(defun |fmax| (x y)
  (max x y))                            ; no error

(defun |fmin| (x y)
  (min x y))                            ; no error

;;; TODO: 'fdim'

;;; TODO: 'nan'

(defun |exp| (x)
  (exp x))               ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

(defun |exp2| (x)        ; C99
  (expt 2 x))            ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

(defun |expm1| (x)
  (exp-1 x))

(defun |log| (x)
  (log x))          ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

(defun |log10| (x)
  (log x 10))       ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

(defun |log2| (x)   ; C99
  (log x 2))       ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

(defun |log1p| (x)
  (log1+ x))

(defun |pow| (x y)
  (expt x y)) ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO, FE_UNDERFLOW, FE_OVERFLOW

(defun |sqrt| (x)
  (sqrt x))                             ; may raise EDOM, FE_INVALID

(defun |cbrt| (x)
  ;; TODO: FIXME: I must check its accuracy.
  (expt x 1/3))                         ; may raise EDOM, FE_INVALID

(defun |hypot| (x y)
  (hypot x y))

(defun |sin| (x)
  (sin x))                              ; may raise EDOM, FE_INVALID

(defun |cos| (x)
  (cos x))                              ; may raise EDOM, FE_INVALID

(defun |tan| (x)
  (tan x))                   ; may raise EDOM, FE_INVALID, FE_OVERFLOW

(defun |asin| (x)
  (asin x))                             ; may raise EDOM, FE_INVALID

(defun |acos| (x)
  (acos x))                             ; may raise EDOM, FE_INVALID

(defun |atan| (x)
  (atan x))                             ; no error

(defun |atan2| (x y)
  (atan x y))                           ; no error

(defun |sinh| (x)
  (sinh x))                             ; may raise ERANGE, FE_INVALID

(defun |cosh| (x)
  (cosh x))                             ; may raise ERANGE, FE_INVALID

(defun |tanh| (x)
  (tanh x))                             ; no error

(defun |asinh| (x)
  (asinh x))                            ; no error

(defun |acosh| (x)
  (acosh x))                            ; may raise EDOM, FE_INVALID

(defun |atanh| (x)
  (atanh x))        ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

;;; TODO: 'erf'
;;; TODO: 'erfc'
;;; TODO: 'tgamma'
;;; TODO: 'lgamma'

(defun |ceil| (x)
  (fceiling x))                            ; no error

(defun |floor| (x)
  (ffloor x))                            ; no error

(defun |trunc| (x)                      ; C99
  (ftruncate x))                        ; no error

(defun |round| (x)                      ; C99
  (fround x))                           ; no error

;;; TODO: 'lround', 'llround'

;;; TODO: 'nearbyint'

;;; TODO: 'rint', 'lrint', 'llrint'

(defun frexp* (x)                      ; FIXME: how to treat pointer? (cons as a storage?)
  (assert (= 2 (float-radix x)))
  (multiple-value-bind (significant exponent sign)
      (decode-float x)
    (values (* significant sign)
            exponent)))

(defun |ldexp| (x exp)
  (assert (= 2 (float-radix x)))
  (scale-float x exp))                  ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

(defun modf* (x)                       ; FIXME: how to treat pointer? (cons as a storage?)
  (multiple-value-bind (quot rem) (ftruncate x)
    (values rem quot)))

(defun |scalbn| (x exp)                 ; C99
  (scale-float x exp))

(defun |ilogb| (x)                      ; C99
  (nth-value 1 (decode-float x)))

(defconstant FP_ILOGB0                  ; C99
  (|ilogb| 0d0))

;;; TODO: FP_ILOGBNAN

(defun |logb| (x)                       ; C99
  (float (nth-value 1 (decode-float x)) x))

;;; TODO: 'nextafter', 'nexttoward'

(defun |copysign| (abs sign)            ; C99
  (float-sign sign abs))

;;; INFINITY (C99)

(defconstant HUGE_VAL
  double-float-positive-infinity)

(defconstant HUGE_VALF                  ; C99
  single-float-positive-infinity)

(defconstant HUGE_VALL                  ; C99
  long-float-positive-infinity)

(defconstant INFINITY                  ; C99
  single-float-positive-infinity)

;; TODO: 'NAN'

;;; FPCLASSIFY (C99)

(defun |isnan| (x)
  (float-nan-p x))

(defun |isinf| (x)
  (if (float-infinity-p x)
      (signum x)))

(defun |isfinite| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x))))

(defun denormalized-float-p (x)
  (typecase x
    (short-float
     (< least-negative-normalized-short-float x least-positive-normalized-short-float))
    (single-float
     (< least-negative-normalized-single-float x least-positive-normalized-single-float))
    (double-float
     (< least-negative-normalized-double-float x least-positive-normalized-double-float))
    (long-float
     (< least-negative-normalized-long-float x least-positive-normalized-long-float))
    (t nil)))

(defun |isnormal| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x)
           (denormalized-float-p x))))

(defconstant FP_NAN :FP_NAN)
(defconstant FP_INFINITE :FP_INFINITE)
(defconstant FP_ZERO :FP_ZERO)
(defconstant FP_SUBNORMAL :FP_SUBNORMAL)
(defconstant FP_NORMAL :FP_NORMAL)

(defun |fpclassify| (x)
  (cond ((float-nan-p x) :FP_NAN)
        ((float-infinity-p x) :FP_INFINITE)
        ((zerop x) :FP_ZERO)
        ((denormalized-float-p x) :FP_SUBNORMAL)
        (t :FP_NORMAL)))

;;; SIGNBIT (C99)

(defun |signbit| (x)
  (minusp (float-sign x)))

;;; TODO: isgreater, isgreaterequal, isless, islessequal, islessgreater, isunordered

;;; C++20
;;; I found it in https://cpprefjp.github.io/reference/cmath/lerp.html
;;; and Alexardia has it!
(defun |lerp| (a b v)
  (alexandria:lerp v a b))

;;; TODO:
;;; float_t, double_t, MATH_ERRNO, MATH_ERREXCEPT, math_errhandling
