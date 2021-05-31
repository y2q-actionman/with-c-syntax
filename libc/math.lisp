;;; Notes:
;;; 
;;; * Currently, I define only double-float versions, like C90.
;;;
;;; * If I implement implicit conversions, all `coercef' are should deleted.
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

(defun wcs-raise-fe-exception (fe-constant &key (errno nil))
  (setf |errno|
        (or errno
            (eswitch (fe-constant)
              (FE_DIVBYZERO with-c-syntax.libc:ERANGE)
              (FE_INVALID with-c-syntax.libc:EDOM)
              (FE_OVERFLOW with-c-syntax.libc:ERANGE)
              (FE_UNDERFLOW with-c-syntax.libc:ERANGE))))
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

(defconstant FP_ILOGB0                  ; C99
  (|ilogb| 0)) ; CLHS says `decode-float' returns an integer even for 0. 

(defconstant FP_ILOGBNAN                ; C99
  most-negative-fixnum
  "with-c-syntax definition FP_ILOGBNAN")


(defconstant MATH_ERRNO 1)
(defconstant MATH_ERREXCEPT 2)
(defconstant |math_errhandling| 0) ; TODO: change to 1 when all functions are supported.

;;; TODO: FP_CONTRACT


;;; Classification

(defun float-subnormal-p (x)
  ;; I think I can write it as:
  ;;   (< 0 (abs x) least-positive-normalized-single-float)
  ;; But it raises 'note: deleting unreachable code' on SBCL. Why?
  (and
   (not (float-nan-p x))
   (/= x 0)
   (etypecase x
     (short-float
      (< least-negative-normalized-short-float x least-positive-normalized-short-float))
     (single-float
      (< least-negative-normalized-single-float x least-positive-normalized-single-float))
     (double-float
      (< least-negative-normalized-double-float x least-positive-normalized-double-float))
     (long-float
      (< least-negative-normalized-long-float x least-positive-normalized-long-float)))))

(defun |fpclassify| (x)
  (cond ((float-nan-p x) :FP_NAN)
        ((float-infinity-p x) :FP_INFINITE)
        ((zerop x) :FP_ZERO)
        ((float-subnormal-p x) :FP_SUBNORMAL)
        (t :FP_NORMAL)))

(defun |isfinite| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x))))

(defun |isinf| (x)
  (float-infinity-p x))

(defun |isnan| (x)
  (float-nan-p x))

(defun |isnormal| (x)
  (not (or (float-nan-p x)
           (float-infinity-p x)
           (float-subnormal-p x)
           (zerop x))))

(defun |signbit| (x)
  (minusp (float-sign x)))

;;; Trigonometric

(defmacro with-acos-parameter-check ((x) &body body)
  `(cond ((float-nan-p ,x) ,x)
         ((not (<= -1 ,x 1))
          (wcs-raise-fe-exception FE_INVALID)
          double-float-nan)
         (t ,@body)))

(defun |acos| (x)
  (coercef x 'double-float)
  (with-acos-parameter-check (x)
    (acos x)))

(defun |asin| (x)
  (coercef x 'double-float)
  (with-acos-parameter-check (x)
    (asin x)))

(defun |atan| (x)
  (coercef x 'double-float)
  (cond ((float-nan-p x) x)
        (t (atan x))))

(defun |atan2| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (cond ((float-nan-p x) x)
        ((float-nan-p y) y)
        (t (atan x y))))

(defmacro with-cos-parameter-check ((x) &body body)
  `(cond ((float-nan-p ,x) ,x)
         ((float-infinity-p ,x)
          (wcs-raise-fe-exception FE_INVALID)
          double-float-nan)
         (t ,@body)))

(defun |cos| (x)
  (coercef x 'double-float)
  (with-cos-parameter-check (x)
    (cos x)))

(defun |sin| (x)
  (coercef x 'double-float)
  (with-cos-parameter-check (x)
    (sin x)))

(defun |tan| (x)
  (coercef x 'double-float)
  (cond ((float-nan-p x) x)
        ((float-infinity-p x)
         (wcs-raise-fe-exception FE_INVALID)
         double-float-nan)
        (t
         (handler-case (tan x)
           (floating-point-overflow ()
             (wcs-raise-fe-exception FE_OVERFLOW)
             HUGE_VAL)))))

(defun |acosh| (x)
  (coercef x 'double-float)
  (cond ((float-nan-p x) x)
        ((< x 1)
         (wcs-raise-fe-exception FE_INVALID)
         double-float-nan)
        (t (acosh x))))

(defun |asinh| (x)
  (coercef x 'double-float)
  (asinh x))                            ; no error

(defun |atanh| (x)
  (coercef x 'double-float)
  (let ((ret
          (handler-case (atanh x)
            (simple-error (e)  ; Allegro CL 10.1 on MacOSX comes here.
              (cond ((or (= x 1.0d0)
                         (= x -1.0d0))
                     (wcs-raise-fe-exception FE_DIVBYZERO)
                     (float-sign x double-float-positive-infinity))
                    (t (error e)))))))
    (cond
      ((complexp ret)
       (wcs-raise-fe-exception FE_INVALID)
       double-float-nan)
      (t ret))))

(defun check-cosh-result (ret x)
  (cond
    ((float-infinity-p ret)
     (unless (float-infinity-p x)
       (wcs-raise-fe-exception FE_OVERFLOW))
     ret)
    (t ret)))

(defun |cosh| (x)
  (coercef x 'double-float)
  (let ((ret (cosh x)))
    (check-cosh-result ret x)
    ret))

(defun |sinh| (x)
  (coercef x 'double-float)
  (let ((ret (sinh x)))
    (check-cosh-result ret x)
    ret))

(defun |tanh| (x)
  (coercef x 'double-float)
  (tanh x))                             ; no error

;;; Exponential and logarithmic

(defmacro with-exp-family-error-handling ((x underflow-value) &body body)
  (once-only (underflow-value)
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
  (coercef x 'double-float)
  (with-exp-family-error-handling (x (float 0 x))
    (exp x)))

(defun |exp2| (x)                       ; C99
  (coercef x 'double-float)
  (with-exp-family-error-handling (x (float 0 x))
    (expt 2 x)))

(defun |expm1| (x)
  (coercef x 'double-float)
  (with-exp-family-error-handling (x (float -1 x))
    (float (exp-1 x) x)))

(defun frexp* (x)  ; FIXME: how to treat pointer? (cons as a storage?)
  (coercef x 'double-float)
  (assert (= 2 (float-radix x)))
  (handler-bind
      ((simple-error
         (lambda (condition)
           ;; Allegro CL 10.0 on MacOS comes here for Inf of NaN.
           (cond ((or (float-nan-p x)
                      (float-infinity-p x))
                  (return-from frexp* x))
                 (t condition)))))
    (multiple-value-bind (significant exponent sign)
        (decode-float x)
      (values (float-sign sign significant)
              exponent))))

(defun |ilogb| (x)                      ; C99
  (coercef x 'double-float)
  (cond ((float-nan-p x)
         (wcs-raise-fe-exception FE_INVALID)
         FP_ILOGBNAN)
        ((float-infinity-p x)
         (wcs-raise-fe-exception FE_INVALID)
         INT_MAX)
        ((zerop x)
         (wcs-raise-fe-exception FE_INVALID)
         FP_ILOGB0)
        (t
         (let ((expon (nth-value 1 (decode-float x))))
           ;; Common Lisp normalizes the significant to [1/radix ~ 1), see `decode-float'.
           ;; C99 normalizes it to [1 ~ FLT_RADIX), see the description of logb() function,
           (1- expon)))))

(defun |ldexp| (x exp)
  (coercef x 'double-float)
  (assert (= 2 (float-radix x)))
  (|scalbn| x exp))

(defmacro with-log-family-parameter-check ((x pole) &body body)
  (once-only (pole)
    `(cond ((float-nan-p ,x) ,x)
           ((= ,x ,pole)
            (wcs-raise-fe-exception FE_DIVBYZERO)
            (- HUGE_VAL))
           ((< ,x ,pole)
            (wcs-raise-fe-exception FE_INVALID)
            double-float-nan)
           ((float-infinity-p ,x) ,x)
           (t ,@body))))

(defun |log| (x)
  (coercef x 'double-float)
  (with-log-family-parameter-check (x (float 0 x))
    (log x)))

(defun |log10| (x)
  (coercef x 'double-float)
  (with-log-family-parameter-check (x (float 0 x))
    (log x 10)))

(defun |log1p| (x)                      ; C99
  (coercef x 'double-float)
  (with-log-family-parameter-check (x (float -1 x))
    (log1+ x)))

(defun |log2| (x)                       ; C99
  (coercef x 'double-float)
  (with-log-family-parameter-check (x (float 0 x))
    (log x 2)))

(defun |logb| (x)                       ; C99
  (coercef x 'double-float)
  (cond ((float-nan-p x) x)
        ((float-infinity-p x) x)
        ((zerop x)
         (wcs-raise-fe-exception FE_DIVBYZERO :errno EDOM)
         double-float-negative-infinity)
        (t
         (let ((expon (nth-value 1 (decode-float x))))
           ;; See `|ilogb|'.
           (float (1- expon) x)))))

(defun modf* (x)   ; FIXME: how to treat pointer? (cons as a storage?)
  (coercef x 'double-float)
  (handler-bind
      ((simple-error
         (lambda (condition)
           ;; Allegro CL 10.0 on MacOS comes here for Inf of NaN.
           (cond ((float-nan-p x)
                  (return-from modf* (values x x)))
                 ((float-infinity-p x)
                  (return-from modf*
                    (values (float-sign x 0d0) x)))
                 (t condition)))))
    (multiple-value-bind (quot rem) (ftruncate x)
      (values rem quot))))

(defun |scalbn| (x exp)                 ; C99
  (coercef x 'double-float)
  (let ((ret
          (handler-bind
              ((simple-error
                 (lambda (condition)
                   ;; Allegro CL 10.0 on MacOS comes here for Inf of NaN.
                   (cond ((or (float-nan-p x)
                              (float-infinity-p x))
                          (return-from |scalbn| x))
                         (t condition)))))
            (scale-float x exp))))
    (cond
      ((and (float-infinity-p ret)
            (not (float-infinity-p x)))
       (wcs-raise-fe-exception FE_OVERFLOW))
      ((and (zerop ret)
            (not (zerop x)))
       (wcs-raise-fe-exception FE_UNDERFLOW)))
    ret))

(defun |scalbln| (x exp)                ; C99
  (|scalbn| x exp))                     ; Because we have BigNum.

;;; Power and absolute-value 

(defun |cbrt| (x)                       ; C99
  (coercef x 'double-float)
  (cond ((float-nan-p x) x)
        ((float-infinity-p x) x)        ; For treating -Inf.
        ((zerop x) x)                   ; For treating -0.0.
        (t
         (when (minusp x)
           (warn "Current cbrt() implementation returns a principal complex value, defined in ANSI CL, for minus parameters."))
         (expt x 1/3))))

(defun |fabs| (x)
  (coercef x 'double-float)
  (abs x))                              ; no error

(defun |hypot| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (cond ((or (float-infinity-p x)
             (float-infinity-p y))
         double-float-positive-infinity)
        ((float-nan-p x) x)
        ((float-nan-p y) y)
        (t
         (let ((ret
                 (handler-case (hypot x y)
                   (floating-point-overflow ()
                     (wcs-raise-fe-exception FE_OVERFLOW)
                     HUGE_VAL)
                   (floating-point-underflow ()
                     (wcs-raise-fe-exception FE_UNDERFLOW :errno nil)
                     +0))))
           (cond ((float-infinity-p ret)
                  (wcs-raise-fe-exception FE_OVERFLOW)
                  HUGE_VAL))
           ret))))

;;; pow()

(defun pow-result-significantly-complex-p (ret x y)
  (declare (type double-float x y)
           (type (complex double-float) ret))
  (let* ((realpart (realpart ret))
         (scaled-epsilon
           (* 2 double-float-epsilon (max (abs x) (abs y) (abs realpart)))))
    (> (imagpart ret) scaled-epsilon)))

(defun |pow| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (labels ((handle-div-0 (&optional _)
             (declare (ignore _))
             (wcs-raise-fe-exception FE_DIVBYZERO)
             (return-from |pow|
               double-float-positive-infinity))
           (handle-invalid (&optional _)
             (declare (ignore _))
             (wcs-raise-fe-exception FE_INVALID)
             (return-from |pow| double-float-nan))
           (handle-overflow (&optional _ (ret double-float-positive-infinity))
             (declare (ignore _))
             (unless (float-infinity-p x)
               (wcs-raise-fe-exception FE_OVERFLOW))
             (return-from |pow| ret))
           (handle-underflow (&optional _)
             (declare (ignore _))
             (unless (float-infinity-p x)
               (wcs-raise-fe-exception FE_UNDERFLOW))
             (return-from |pow| 0d0))
           (handle-simple-error (condition)
             ;; Allegro CL 10.0 on MacOSX comes here -- (expt 0.0 -1).
             (cond ((and (zerop x) (minusp y))
                    (handle-div-0 condition))
                   (t condition))))
    (let ((ret
            (handler-bind
                ((division-by-zero #'handle-div-0)
                 (floating-point-invalid-operation #'handle-invalid)
                 (floating-point-overflow #'handle-overflow)
                 (floating-point-underflow #'handle-underflow)
                 (simple-error #'handle-simple-error))
              (expt x y))))
      (cond
        ((complexp ret)
         (let ((realpart (realpart ret)))
           (cond
             ((float-nan-p realpart)
              ;; (expt -1 double-float-negative-infinity) comes here, on Allegro CL 10.1 on MacOS X
              (handle-invalid))
             ((float-infinity-p realpart)
              (handle-overflow nil realpart))
             ((pow-result-significantly-complex-p ret x y)
              (handle-invalid))
             (t
              ;; Allegro CL comes here, even if (plusp y).
              ;;   (expt -1.1 2.0) -> #C(1.2100000000000002d0 -2.963645253936595d-16)
              ;; FIXME: I am suspicious about this routine.
              ;;   (Can I utilize `floating-point:relative-error' ?)
              realpart))))
        (t ret)))))

(defun |sqrt| (x)
  (coercef x 'double-float)
  (flet ((handle-simple-error (condition)
           ;; Allegro CL 10.0 on MacOS comes here; (sqrt double-float-nan).
           (cond ((float-nan-p x)
                  (return-from |sqrt| x))
                 (t condition))))
    (let ((ret
            (handler-bind
                ((simple-error #'handle-simple-error))
              (sqrt x))))
      (cond
        ((complexp ret)
         (wcs-raise-fe-exception FE_INVALID)
         double-float-nan)
        (t ret)))))

;;; TODO: 'erf()', 'erfc()', 'tgamma()', 'lgamma()'

;;; Nearest Integer

(defmacro with-nearest-int-error-handling ((x) &body body)
  (with-gensyms (block-name)
    `(block ,block-name
       (handler-bind
           ((simple-error
              (lambda (condition) 
                ;; Allegro CL 10.0 on MacOS comes here; (fceiling double-float-nan).
                (cond ((or (float-nan-p ,x)
                           (float-infinity-p ,x))
                       (return-from ,block-name ,x))
                      (t condition)))))
         ,@body))))

(defun |ceil| (x)
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (fceiling x)))

(defun |floor| (x)
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (ffloor x)))

;;; TODO: 'nearbyint()'
;;; TODO: 'rint()', 'lrint()', 'llrint()'

(defun |round| (x)                      ; C99
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (fround x)))

(defun |lround| (x)                     ; C99
  (coercef x 'double-float)
  (handler-bind
      ((simple-error
         (lambda (condition) 
           ;; Allegro CL 10.0 on MacOS comes here; (fceiling double-float-nan).
           (cond ((float-nan-p x)
                  (wcs-raise-fe-exception FE_INVALID)
                  (return-from |lround| most-positive-fixnum))
                 ((float-infinity-p x)
                  (wcs-raise-fe-exception FE_OVERFLOW)
                  (return-from |lround| most-positive-fixnum))
                 (t condition)))))
    (round x)))

(defun |llround| (x)                    ; C99
  (|lround| x))                         ; Because we have BigNum.

(defun |trunc| (x)                      ; C99
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (ftruncate x)))

(defmacro with-mod-family-parameter-check ((x y) &body body)
  `(cond ((float-nan-p ,x) ,x)
         ((float-nan-p ,y) ,y)
         ((or (float-infinity-p ,x)
              (zerop ,y))
          (wcs-raise-fe-exception FE_INVALID)
          double-float-nan)
         (t ,@body)))

;;; Remainder

(defun |fmod| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-parameter-check (x y)
    (if (zerop x)
        x
        (nth-value 1 (ftruncate x y)))))

(defun |remainder| (x y)                ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-parameter-check (x y)
    (nth-value 1 (fround x y))))

(defun remquo* (x y)                    ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-parameter-check (x y)
    (multiple-value-bind (quotient remainder)
        (round x y)
      (values remainder quotient))))

;;; TODO: real 'remquo' -- support pointer passing..

;;; Manipulation

(defun |copysign| (abs sign)            ; C99
  (coercef abs 'double-float)
  (coercef sign 'double-float)
  (float-sign sign abs))

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
  (coercef x 'double-float)
  (coercef y 'double-float)
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
                  (float-subnormal-p ret))
              (wcs-raise-fe-exception FE_UNDERFLOW)))
       ret))))

;;; TODO: nexttoward()

;;; Maximum, minimum, and positive difference

(defun |fdim| (x y)                     ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
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
  `(cond
     ((float-nan-p ,y) ,x) ; If X is a NaN, just the NaN of X is returned. I prioritize X over Y.
     ((float-nan-p ,x) ,y)
     (t ,@body)))

(defun |fmax| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-fmax-fmin-parameter-check (x y)
    (max x y)))

(defun |fmin| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-fmax-fmin-parameter-check (x y)
    (min x y)))

;;; TODO: 'fma'








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
