(in-package #:with-c-syntax.libc-implementation)

;;; I define only type-generic functions like <tgmath.h>,
;;; The numeric functions of Common Lisp are already so.

(defun |fabs| (x)
  (abs x))                              ; no error

(defun |fmod| (x y)
  (nth-value 1 (ftruncate x y)))        ; may raise EDOM, FE_INVALID

(defun |remainder| (x y)                ; C99
  (nth-value 1 (fround x y)))           ; may raise EDOM, FE_INVALID

(defun |fmax| (x y)
  (max x y))                            ; no error

(defun |fmin| (x y)
  (min x y))                            ; no error

(defun |exp| (x)
  (exp x))               ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

(defun |exp2| (x)        ; C99
  (expt 2 x))            ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

;;; expm1 is in floating-point-contractions

(defun |log| (x)
  (log x))          ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

(defun |log10| (x)
  (log x 10))       ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

(defun |log2| (x)   ; C99
  (log x 2))       ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO

;;; log1p is in floating-point-contractions

(defun |pow| (x y)
  (expt x y)) ; may raise EDOM, ERANGE, FE_INVALID, FE_DIVBYZERO, FE_UNDERFLOW, FE_OVERFLOW

(defun |sqrt| (x)
  (sqrt x))                             ; may raise EDOM, FE_INVALID

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

(defun |ceil| (x)
  (fceiling x))                            ; no error

(defun |floor| (x)
  (ffloor x))                            ; no error

(defun |trunc| (x)                      ; C99
  (ftruncate x))                        ; no error

(defun |round| (x)                      ; C99
  (fround x))                           ; no error

(defun |frexp| (x)                      ; FIXME: how to treat pointer? (cons as a storage?)
  (assert (= 2 (float-radix x)))
  (multiple-value-bind (significant exponent sign)
      (decode-float x)
    (values (* significant sign)
            exponent)))

(defun |ldexp| (x exp)
  (assert (= 2 (float-radix x)))
  (scale-float x exp))                  ; may raise ERANGE, FE_OVERFLOW, FE_UNDERFLOW

(defun |modf| (x)                       ; FIXME: how to treat pointer? (cons as a storage?)
  (ftruncate x))

(defun |scalbn| (x exp)                 ; C99
  (scale-float x exp))

(defun |ilogb| (x)                      ; C99
  (nth-value 1 (decode-float x)))

(defun |logb| (x)                       ; C99
  (float (nth-value 1 (decode-float x)) x))

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

;; sadly, float-features does not provide NAN constant.

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

(defun |fpclassify| (x)
  (cond ((float-nan-p x) :FP_NAN)
        ((float-infinity-p x) :FP_INFINITE)
        ((zerop x) :FP_ZERO)
        ((denormalized-float-p x) :FP_SUBNORMAL)
        (t :FP_NORMAL)))

(defun |isnormal| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x))))

;;; SIGNBIT (C99)

(defun |signbit| (x)
  (minusp (float-sign x)))

;;; C++20

;;; I found it in https://cpprefjp.github.io/reference/cmath/lerp.html
;;; and Alexardia has it!
(defun |lerp| (a b v)
  (alexandria:lerp v a b))
