;;; Notes:
;;; 
;;; * Currently, I define only double-float versions, like C90.
;;; 
;;; * If I implement <tgmath.h>, I will  define 'f' and 'l' variants,
;;;   and write a compiler-macro to use them.
;;; 
;;; * When using NaN, many mathematical functions of Common Lisp
;;; behave differently from C99. So I manually handle these
;;; situations..
;;; 
;;; * If a NaN passed to parameters, I try to save it to the result.

(in-package #:with-c-syntax.libc-implementation)

;;; Utils

(defun wcs-raise-fe-exception (fe-constant)
  (eswitch (fe-constant)
    (FE_DIVBYZERO
     (error "FIXME"))
    (FE_INVALID
     (setf |errno| 'with-c-syntax.libc:EDOM))
    (FE_OVERFLOW
     (setf |errno| 'with-c-syntax.libc:ERANGE))
    (FE_UNDERFLOW
     (setf |errno| 'with-c-syntax.libc:ERANGE)))
  (|feraiseexcept| fe-constant))

;;; TODO: libc symbols should be arranged by the order in C99 (ISO/IEC 9899).

(locally
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (case FLT_EVAL_METHOD
    (0
     (add-typedef '|float_t| 'single-float)
     (add-typedef '|double_t| 'double-float))
    (1
     (add-typedef '|float_t| 'double-float)
     (add-typedef '|double_t| 'double-float))
    (2
     (add-typedef '|float_t| 'long-float)
     (add-typedef '|double_t| 'long-float))
    (otherwise
     (warn "Please consult the author about `FLT_EVAL_METHOD'.")
     (add-typedef '|float_t| 'single-float)
     (add-typedef '|double_t| 'double-float))))

;;; INFINITY (C99)
(defconstant HUGE_VAL double-float-positive-infinity)
(defconstant HUGE_VALF single-float-positive-infinity) ; C99
(defconstant HUGE_VALL long-float-positive-infinity)   ; C99
(defconstant INFINITY single-float-positive-infinity)  ; C99
(defconstant NAN double-float-nan)                     ; C99

;;; FPCLASSIFY constants (C99)
(defconstant FP_INFINITE :FP_INFINITE)
(defconstant FP_NAN :FP_NAN)
(defconstant FP_NORMAL :FP_NORMAL)
(defconstant FP_SUBNORMAL :FP_SUBNORMAL)
(defconstant FP_ZERO :FP_ZERO)

;;; TODO: FP_FAST_FMA (...in far future)

;;; TODO: FP_ILOGB0, FP_ILOGBNAN

(defconstant MATH_ERRNO 1)
(defconstant MATH_ERREXCEPT 2)
(defconstant |math_errhandling| 0) ; TODO: change to 1 when all functions are supported.

;;; TODO: FP_CONTRACT


;;; Functions

(defmacro with-exp-family-error-handling ((x underflow-value) &body body)
  (once-only (x underflow-value)
    `(cond ((float-nan-p ,x) ,x)
           ((float-infinity-p ,x)
            (if (plusp ,x)
                double-float-positive-infinity
                ,underflow-value))
           (t
            ;; This is for Allegro.
            ;; `with-float-traps-masked' may also be used.
            (handler-case
                (let ((ret (progn ,@body)))
                  (cond ((float-infinity-p ret)
                         (wcs-raise-fe-exception FE_OVERFLOW))
                        ((= ret ,underflow-value)
                         (wcs-raise-fe-exception FE_UNDERFLOW)))
                  ret)
              (floating-point-overflow ()
                (wcs-raise-fe-exception FE_OVERFLOW)
                HUGE_VAL)
              (floating-point-underflow ()
                (wcs-raise-fe-exception FE_UNDERFLOW)
                ,underflow-value))))))

(defun |exp| (x)
  (declare (type double-float x))
  (with-exp-family-error-handling (x (float 0 x))
    (exp x)))

(defun |exp2| (x)                       ; C99
  (declare (type double-float x))
  (with-exp-family-error-handling (x (float 0 x))
    (expt 2 x)))

(defun |expm1| (x)
  (declare (type double-float x))
  (with-exp-family-error-handling (x (float -1 x))
    (float (exp-1 x) x)))

(defun |fabs| (x)
  (declare (type double-float x))
  (abs x))                              ; no error

(defmacro with-mod-family-parameter-check ((x y) &body body)
  (once-only (x y)
    `(cond ((float-nan-p ,x) ,x)
           ((float-nan-p ,y) ,y)
           ((or (float-infinity-p ,x)
                (zerop ,y))
            (wcs-raise-fe-exception FE_INVALID)
            double-float-nan)
           (t ,@body))))

(defun |fmod| (x y)
  (declare (type double-float x y))
  (with-mod-family-parameter-check (x y)
    (if (zerop x)
        x
        (nth-value 1 (ftruncate x y)))))

(defun |remainder| (x y)                ; C99
  (declare (type double-float x y))
  (with-mod-family-parameter-check (x y)
    (nth-value 1 (fround x y))))

(defun remquo* (x y)                    ; C99
  (declare (type double-float x y))
  (with-mod-family-parameter-check (x y)
    (multiple-value-bind (quotient remainder)
        (round x y)
      (values remainder quotient))))

;;; TODO: real 'remquo' -- support pointer passing..

(defun |nan| (&optional (tagp ""))      ; C99
  (cond
    ((string= tagp "") double-float-nan)
    (t
     ;; Uses TAGP for specifing lower bits of NAN.
     ;; This is my experiment.
     (let* ((lower-bits-int (or (parse-integer tagp :junk-allowed t)
                                0))
            (bitmask #b11111111111111111111111) ; 23 bits.
            (nan-bits (double-float-bits double-float-nan))
            (new-nan-bits (logior (logand lower-bits-int bitmask)
                                  nan-bits)))
       (bits-double-float new-nan-bits)))))

;;; nextafter (C99)

(defun step-double-float-plus (x)
  (cond
    ((float-nan-p x) x)
    ((float-infinity-p x)
     (if (plusp x)
         x
         most-negative-double-float))
    ((zerop x)                          ; '+0' or '-0'
     least-positive-double-float)
    (t
     ;; This implementation bases on Binary64 format of IEEE-754.
     ;; (I tried `integer-decode-float' firstly. It worked for
     ;;  normalized floats, but subnormals are quite difficult.)
     (let ((bits (double-float-bits x)))
       (if (plusp x)
           (incf bits)
           (decf bits))
       (bits-double-float bits)))))

(defun step-double-float-minus (x)
  (cond
    ((float-nan-p x) x)
    ((float-infinity-p x)
     (if (plusp x)
         most-positive-double-float
         x))
    ((zerop x)
     least-negative-double-float)
    (t
     (let ((bits (double-float-bits x)))
       (if (plusp x)
           (decf bits)
           (incf bits))
       (bits-double-float bits)))))

(defun |nextafter| (x y)
  (declare (type double-float x y))
  (cond
    ((float-nan-p x) x)
    ((float-nan-p y) y)
    ((= x y) y)
    (t
     (let ((ret (if (< x y)
                    (step-double-float-plus x)
                    (step-double-float-minus x))))
       (cond ((and (float-infinity-p ret)
                   (not (float-infinity-p x)))
              (wcs-raise-fe-exception FE_OVERFLOW))
             ((or (zerop ret)
                  (denormalized-float-p ret))
              (wcs-raise-fe-exception FE_UNDERFLOW)))
       ret))))

(defun |fdim| (x y)                     ; C99
  (declare (type double-float x y))
  (cond
    ((float-nan-p x) x)
    ((float-nan-p y) y)
    ((<= x y) (float 0 x))
    (t
     (handler-case
         (let ((diff (- x y)))
           (cond ((and (not (|isfinite| diff))
                       (not (float-infinity-p x))
                       (not (float-infinity-p y)))
                  (wcs-raise-fe-exception FE_OVERFLOW)
                  HUGE_VAL)
                 (t
                  diff)))
       (floating-point-overflow ()
         (wcs-raise-fe-exception FE_OVERFLOW)
         HUGE_VAL)))))

(defmacro with-fmax-fmin-parameter-check ((x y) &body body)
  (once-only (x y)
    `(cond
       ((float-nan-p ,y) ,x) ; If X is a NaN, just the NaN of X is returned. I prioritize X over Y.
       ((float-nan-p ,x) ,y)
       (t ,@body))))

(defun |fmax| (x y)
  (declare (type double-float x y))
  (with-fmax-fmin-parameter-check (x y)
    (max x y)))

(defun |fmin| (x y)
  (declare (type double-float x y))
  (with-fmax-fmin-parameter-check (x y)
    (min x y)))

;;; TODO: 'fma'


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
  (if (floatp x)
      (if (or (float-infinity-p x)
              (float-nan-p x))
          x
          (ffloor x))
      (cl:floor x)))

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
  (nth-value 1 (decode-float 0d0)))

;;; TODO: FP_ILOGBNAN

(defun |logb| (x)                       ; C99
  (float (nth-value 1 (decode-float x)) x))

;;; TODO: 'nextafter', 'nexttoward'

(defun |copysign| (abs sign)            ; C99
  (float-sign sign abs))

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
  (etypecase x
    (short-float
     (or (< least-negative-normalized-short-float x 0)
         (< 0 x least-positive-normalized-short-float)))
    (single-float
     (or (< least-negative-normalized-single-float x 0)
         (< 0 x least-positive-normalized-single-float)))
    (double-float
     (or (< least-negative-normalized-double-float x 0)
         (< 0 x least-positive-normalized-double-float)))
    (long-float
     (or (< least-negative-normalized-long-float x 0)
         (< 0 x least-positive-normalized-long-float)))))

(defun |isnormal| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x)
           (denormalized-float-p x))))

(defun |fpclassify| (x)
  (cond ((float-nan-p x) :FP_NAN)
        ((float-infinity-p x) :FP_INFINITE)
        ((zerop x) :FP_ZERO)
        ((denormalized-float-p x) :FP_SUBNORMAL)
        (t :FP_NORMAL)))

;;; SIGNBIT (C99)

(defun |signbit| (x)
  (minusp (float-sign x)))

(defun |isgreater| (x y)        ; FIXME: In C99, this must be a macro.
  (if (|isunordered| x y)
      nil
      (> x y)))

(defun |isgreaterequal| (x y)   ; FIXME: In C99, this must be a macro.
  (if (|isunordered| x y)
      nil
      (>= x y)))

(defun |isless| (x y)           ; FIXME: In C99, this must be a macro.
  (if (|isunordered| x y)
      nil
      (< x y)))

(defun |islessequal| (x y)      ; FIXME: In C99, this must be a macro.
  (if (|isunordered| x y)
      nil
      (<= x y)))

(defun |islessgreater| (x y)    ; FIXME: In C99, this must be a macro.
  (if (|isunordered| x y)
      nil
      (/= x y)))

(defun |isunordered| (x y)      ; FIXME: In C99, this must be a macro.
  (or (float-nan-p x) (float-nan-p y)))

(define-compiler-macro |isunordered| (x y)
  `(locally
       (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     (or (float-nan-p ,x) (float-nan-p ,y))))

;;; C++20
;;; I found it in https://cpprefjp.github.io/reference/cmath/lerp.html
;;; and Alexardia has it!
(defun |lerp| (a b v)
  (alexandria:lerp v a b))

;;; TODO:
;;; MATH_ERRNO, MATH_ERREXCEPT, math_errhandling
