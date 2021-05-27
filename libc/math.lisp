;;; Notes:
;;; 
;;; * Currently, I define only double-float versions, like C90.
;;; 
;;; * When using NaN, many mathematical functions of Common Lisp
;;; behave differently from C99. So I manually handle these
;;; situations..
;;; 
;;; * If a NaN passed to parameters, I try to save it to the result.

(in-package #:with-c-syntax.libc-implementation)

;;; TODO: These should be arranged by the order in C99 (ISO/IEC 9899).

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
            (let ((ret
                    ;; This is for Allegro.
                    ;; `with-float-traps-masked' may also be used.
                    (handler-case (progn ,@body)
                      (floating-point-overflow ()
                        (setf |errno| 'with-c-syntax.libc:ERANGE)
                        ;; TODO: raise FE_OVERFLOW
                        HUGE_VAL)
                      (floating-point-underflow ()
                        (setf |errno| 'with-c-syntax.libc:ERANGE)
                        ;; TODO: raise FE_UNDERFLOW
                        ,underflow-value))))
              (cond ((float-infinity-p ret)
                     ;; TODO: raise FE_OVERFLOW
                     (setf |errno| 'with-c-syntax.libc:ERANGE))
                    ((zerop ret)
                     ;; TODO: raise FE_UNDERFLOW
                     (setf |errno| 'with-c-syntax.libc:ERANGE)))
              ret)))))

(defun |exp| (x)
  (declare (type double-float x))
  (with-exp-family-error-handling (x 0.0d0)
    (exp x)))

(defun |exp2| (x)                       ; C99
  (declare (type double-float x))
  (with-exp-family-error-handling (x 0.0d0)
    (expt 2 x)))

(defun |expm1| (x)
  (declare (type double-float x))
  (with-exp-family-error-handling (x -1.0d0)
    (exp-1 x)))

(defun |fabs| (x)
  (declare (type double-float x))
  (abs x))                              ; no error

(defun |fmod| (x y)
  (declare (type double-float x y))
  (cond ((float-nan-p x) x)
        ((float-nan-p y) y)
        ((or (float-infinity-p x) (zerop y))
         (setf |errno| 'with-c-syntax.libc:EDOM)
         double-float-nan)
        ((zerop x)
         x)
        (t
         (nth-value 1 (ftruncate x y)))))

(defun |remainder| (x y)                ; C99
  (declare (type double-float x y))
  (cond ((float-nan-p x) x)
        ((float-nan-p y) y)
        ((float-infinity-p x)
         ;; TODO: raise FE_INVALID
         double-float-nan)
        ((zerop y)
         (setf |errno| 'with-c-syntax.libc:EDOM)
         ;; TODO: raise FE_INVALID
         double-float-nan)
        (t
         (nth-value 1 (fround x y)))))

(defun remquo* (x y)                    ; C99
  (declare (type double-float x y))
  (cond ((float-nan-p x) x)
        ((float-nan-p y) y)
        ((or (float-infinity-p x)
             (zerop y))
         ;; TODO: raise FE_INVALID
         double-float-nan)
        (t
         (multiple-value-bind (quotient remainder)
             (round x y)
           (values remainder quotient)))))

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

(defun |fdim| (x y)                     ; C99
  (declare (type double-float x y))
  (cond ((float-nan-p x) x)
        ((float-nan-p y) y)
        ((<= x y) 
         0.0d0)
        (t
         (let ((diff (- x y)))
           (cond ((and (not (|isfinite| diff))
                       (not (float-infinity-p x))
                       (not (float-infinity-p y)))
                  (setf |errno| 'with-c-syntax.libc:ERANGE)
                  HUGE_VAL)
                 (t
                  diff))))))

(defmacro with-fmax-fmin-parameter-check ((x y) &body body)
  (once-only (x y)
    `(cond ((float-nan-p ,y)
            ,x)        ; If X is a NaN, just the NaN of X is returned. I prioritize X over Y.
           ((float-nan-p ,x)
            ,y)
           (t
            ,@body))))

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
  `(or (float-nan-p ,x) (float-nan-p ,y)))

;;; C++20
;;; I found it in https://cpprefjp.github.io/reference/cmath/lerp.html
;;; and Alexardia has it!
(defun |lerp| (a b v)
  (alexandria:lerp v a b))

;;; TODO:
;;; MATH_ERRNO, MATH_ERREXCEPT, math_errhandling
