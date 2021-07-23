;;; Notes:
;;; 
;;; * Currently, I define only double-float versions, like C90.
;;;
;;; * If I implement implicit conversions, all `coercef' are should deleted.
;;; 
;;; * If I implement <tgmath.h>, I will  define 'f' and 'l' variants,
;;;   and write a compiler-macro to use them.
;;; 
;;; * Implementation strategy:
;;;   1. Call the function.
;;;   2. Handle `arithmetic-error's.
;;;   3. Sees the result; especially complex should be treated.
;;; 
;;; * When using NaN, many mathematical functions of Common Lisp
;;; behave differently from C99. So I manually handle these
;;; situations..
;;; 
;;; * If a NaN passed to parameters, I try to save it to the result.

(in-package #:with-c-syntax.libc-implementation)

;;; About NaN

;;; KLUDGE: compiling NaN causes many problems.
;;;
;;; * SBCL 2.0.5 cannot compile a code includes NaN.
;;;
;;; SBCL 2.0.5 cannot compile the next code:
;;;
;;;   (defun return-nan ()
;;;     float-features:double-float-nan)
;;;
;;; By compiling it, FLOATING-POINT-INVALID-OPERATION will be raised, like
;;;  http://report.quicklisp.org/2021-06-22/failure-report/with-c-syntax.html
;;; This problem led our with-c-syntax to be removed from Quicklisp.
;;;  (https://github.com/y2q-actionman/with-c-syntax/issues/15)
;;;
;;; I noticed using a special variable for NaN can avoid this problem,
;;; so I decided to use this kludge whenever SBCL 2.0.5 is used in
;;; Quicklisp.  SBCL 2.1.4 does not have this problem.
;;;
;;; * ECL 21.2.1 raises an error when `defconstant' is used for NaN.
;;;
;;; ECL 21.2.1 cannot compile the next code:
;;;
;;;   (defconstant NAN double-float-nan)
;;;
;;; Compiling it will show us a error message:
;;;   Error:
;;;     in file math.lisp, position 5690
;;;     at (DEFCONSTANT NAN ...)
;;;     * The form (PROGN (SI:*MAKE-CONSTANT 'NAN DOUBLE-FLOAT-NAN) (SI::REGISTER-GLOBAL 'NAN)) was not evaluated successfully.
;;;
;;; For avoiding this problem, we can uitilize `define-symbol-macro',
;;; like float-features do:
;;;
;;;   (define-symbol-macro NAN double-float-nan)

(defvar NAN
  ;; KLUDGE: `float-features:double-float-nan' fails on ECL because it
  ;; calls `bits-double-float' which does not support ECL.
  #+ecl (ext:nan)
  #-ecl double-float-nan)

;;; Utils

(defun wcs-raise-fe-exception (fe-constant &key (errno nil))
  (setf |errno|
        (or errno
            (eswitch (fe-constant)
              (FE_DIVBYZERO with-c-syntax.libc:EDOM)
              (FE_INVALID with-c-syntax.libc:EDOM)
              (FE_OVERFLOW with-c-syntax.libc:ERANGE)
              (FE_UNDERFLOW with-c-syntax.libc:ERANGE))))
  (|feraiseexcept| fe-constant))

(defun call-with-wcs-math-error-handler
    (function args
     &key
       (underflow-value 0d0)
       &allow-other-keys)
  (flet ((handle-div-0 (&optional _)
           (declare (ignore _))
           (wcs-raise-fe-exception FE_DIVBYZERO)
           (throw 'return-from-with-wcs-math-error-handling
             (values NAN |errno|)))
         (handle-invalid (&optional _)
           (declare (ignore _))
           (when (notany #'float-nan-p args)
             (wcs-raise-fe-exception FE_INVALID))
           (throw 'return-from-with-wcs-math-error-handling
             (values NAN |errno|)))
         (handle-overflow (&optional _)
           (declare (ignore _))
           (when (notany #'float-infinity-p args)
             (wcs-raise-fe-exception FE_OVERFLOW))
           (throw 'return-from-with-wcs-math-error-handling
             (values double-float-positive-infinity |errno|)))
         (handle-underflow (&optional _)
           (declare (ignore _))
           (when (notany #'float-infinity-p args)
             (wcs-raise-fe-exception FE_UNDERFLOW))
           (throw 'return-from-with-wcs-math-error-handling
             (values underflow-value |errno|))))
    (declare (dynamic-extent (function handle-div-0)
                             (function handle-invalid)
                             (function handle-overflow)
                             (function handle-underflow)))
    (handler-bind
        ((division-by-zero #'handle-div-0)
         (floating-point-invalid-operation #'handle-invalid)
         (floating-point-overflow #'handle-overflow)
         (floating-point-underflow #'handle-underflow))
      (apply function args))))

(defun check-wcs-math-result (ret arg-list)
  (cond ((complexp ret)
         (when (notany #'float-nan-p arg-list)
           (wcs-raise-fe-exception FE_INVALID))
         (throw 'return-from-with-wcs-math-error-handling
           (values NAN |errno|)))
        ((not (floatp ret)) ; For SBCL: `float-nan-p' requires a float, but `exp-1' may return -1.
         ret)
        ((float-nan-p ret)
         (when (notany #'float-nan-p arg-list)
           (wcs-raise-fe-exception FE_INVALID))
         (throw 'return-from-with-wcs-math-error-handling
           (values ret |errno|)))
        ((float-infinity-p ret)
         (when (notany #'float-infinity-p arg-list)
           (wcs-raise-fe-exception FE_OVERFLOW))
         (throw 'return-from-with-wcs-math-error-handling
           (values ret |errno|)))
        (t ret)))

(defmacro with-wcs-math-error-handling
    ((var-or-var-list
      (function x &optional (y nil y-supplied-p))
      &rest keyargs
      &key underflow-value)
     &body body)
  (declare (ignorable underflow-value))
  (let ((x_ (gensym)) (y_ (gensym)) (args_ (gensym))
        (var-list (if (listp var-or-var-list)
                      var-or-var-list
                      `(,var-or-var-list))))
    `(let* ((,x_ ,x)
            (,y_ ,y)
            (,args_ (list ,x_ ,@(if y-supplied-p `(,y_)))))
       (declare (ignorable ,y_)
                (type list ,args_)
                (dynamic-extent ,args_))
       (catch 'return-from-with-wcs-math-error-handling
         (multiple-value-bind ,var-list
             (call-with-wcs-math-error-handler ',function ,args_
                                               ,@keyargs)
           (flet ((check-wcs-math-result
                      (&optional (ret ,(first var-list))
                                 (arg-list ,args_))
                    (check-wcs-math-result ret arg-list)))
             ,@body))))))

;;; <math.h> definitions begins here.
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
;; NAN is defined at the top.

;;; FPCLASSIFY constants (C99)
(defconstant FP_INFINITE :FP_INFINITE)
(defconstant FP_NAN :FP_NAN)
(defconstant FP_NORMAL :FP_NORMAL)
(defconstant FP_SUBNORMAL :FP_SUBNORMAL)
(defconstant FP_ZERO :FP_ZERO)

;;; TODO: FP_FAST_FMA (...in far future)

(defconstant FP_ILOGB0                  ; C99
  (1-                                   ; See `|ilogb|'.
   (nth-value 1 (decode-float 0d0)))) ; CLHS says `decode-float' returns an integer even for 0. 

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

(defun |acos| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (acos x))
    (check-wcs-math-result)))

(defun |asin| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (asin x))
    (check-wcs-math-result)))

(defun |atan| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (atan x))
    (check-wcs-math-result)))

(defun |atan2| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-wcs-math-error-handling (ret (atan x y))
    (check-wcs-math-result)))

(defun |cos| (x)
  (coercef x 'double-float)
  (cond
    ((float-nan-p x) x)
    ((float-infinity-p x)               ; for CCL.
     (wcs-raise-fe-exception FE_INVALID)
     NAN)
    (t (with-wcs-math-error-handling (ret (cos x))
         (check-wcs-math-result)))))

(defun |sin| (x)
  (coercef x 'double-float)
  (cond
    ((float-nan-p x) x)
    ((float-infinity-p x)               ; for CCL.
     (wcs-raise-fe-exception FE_INVALID)
     NAN)
    (t (with-wcs-math-error-handling (ret (sin x))
         (check-wcs-math-result)))))

(defun |tan| (x)
  (coercef x 'double-float)
  (cond
    ((float-nan-p x) x)
    ((float-infinity-p x)               ; for CCL.
     (wcs-raise-fe-exception FE_INVALID)
     NAN)
    (t (with-wcs-math-error-handling (ret (tan x))
         (check-wcs-math-result)))))

;;; Hyperbolic

(defun |acosh| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (acosh x))
    (check-wcs-math-result)))

(defun |asinh| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (asinh x))
    (check-wcs-math-result)))           ; no error

(defun |atanh| (x)
  (coercef x 'double-float)
  (cond
    ((float-nan-p x) x) ; For SBCL; This check is required before `cl:='.
    ((or (= x 1.0d0)
         (= x -1.0d0))
     ;; If this check deleted, Allegro CL 10.1 on MacOSX throws `simple-error'.
     (wcs-raise-fe-exception FE_DIVBYZERO :errno ERANGE) ; POSIX says it.
     (float-sign x double-float-positive-infinity))
    (t (with-wcs-math-error-handling (ret (atanh x))
        (check-wcs-math-result)))))

(defun |cosh| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (cosh x))
    (check-wcs-math-result)))

(defun |sinh| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (sinh x))
    (check-wcs-math-result)))

(defun |tanh| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (tanh x))
    (check-wcs-math-result)))           ; no error

;;; Exponential and logarithmic

(defun |exp| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (exp x))
    (check-wcs-math-result)))

(defun |exp2| (x)                       ; C99
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (expt (float 2 x) x))
    (check-wcs-math-result)
    ;; Allegro 10.1 requires this...
    #+ (or sbcl allegro ccl)
    (cond ((= ret 0d0)
           (unless (float-infinity-p x)
             (wcs-raise-fe-exception FE_UNDERFLOW))
           ret)
          (t
           ret))
    #- (or sbcl allegro ccl) 
    ret))

(defun |expm1| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (exp-1 x)
                                     :underflow-value (float -1 x))
    (check-wcs-math-result)
    #+(or sbcl ccl)
    ;; For SBCL and CCL: It does not report `floating-point-underflow'
    (cond
      ((= ret -1d0)
       (wcs-raise-fe-exception FE_UNDERFLOW)
       (float ret x))
      (t ret))
    #-(or sbcl ccl)
    ret))

(defun frexp* (x)  ; FIXME: how to treat pointer? (cons as a storage?)
  (coercef x 'double-float)
  (assert (= 2 (float-radix x)))
  (cond
    ;; If this check deleted, Allegro CL 10.1 on MacOSX throws `simple-error'.
    ((or (float-nan-p x)
         (float-infinity-p x))
     x)
    (t
     (handler-case
         (multiple-value-bind (significant exponent sign)
             (decode-float x)
           (values (float-sign sign significant)
                   exponent))))))

(defun |ilogb| (x)                      ; C99
  (coercef x 'double-float)
  ;; For SBCL: requires it for avoiding `simple-error' and `floating-point-invalid-operation'.
  ;; Allegro CL 10.0 on MacOS raises `simple-error' without it for Inf of NaN.
  (cond ((float-nan-p x)
         (wcs-raise-fe-exception FE_INVALID)
         FP_ILOGBNAN)
        ((float-infinity-p x)
         (wcs-raise-fe-exception FE_INVALID)
         INT_MAX)
        (t
         (let ((expon (nth-value 1 (decode-float x))))
           ;; Common Lisp normalizes the significant to [1/radix ~ 1), see `decode-float'.
           ;; C99 normalizes it to [1 ~ FLT_RADIX), see the description of logb() function,
           (1- expon)))))

(defun |ldexp| (x exp)
  (coercef x 'double-float)
  (assert (= 2 (float-radix x)))
  (|scalbn| x exp))

(defmacro with-log-error-handling (() &body body)
  `(handler-case
      (progn ,@body)
    #+allegro
    ;; TODO: rewrite other `simple-error' handler to use arithmetic-error.
    (arithmetic-error (e)
      (let ((operands (arithmetic-error-operands e)))
        (cond ((zerop (first operands))
               (wcs-raise-fe-exception FE_DIVBYZERO :errno ERANGE)
               (- HUGE_VAL))
              (t
               (error e)))))))

(defun |log| (x)
  (coercef x 'double-float)
  (with-log-error-handling ()
    (with-wcs-math-error-handling (ret (log x))
      (check-wcs-math-result))))

(defun |log10| (x)
  (coercef x 'double-float)
  (with-log-error-handling ()
    (with-wcs-math-error-handling (ret (log x (float 10 x)))
      (check-wcs-math-result))))

(defun |log1p| (x)                      ; C99
  (coercef x 'double-float)
  ;; XXX: Workaround for `floating-point-contractions:log1+'.
  ;; It returns NaN for +Inf.
  (when (float-infinity-p x)
    (return-from |log1p|
      (cond ((plusp x)
             double-float-positive-infinity)
            (t
             (wcs-raise-fe-exception FE_INVALID)
             NAN))))
  (with-log-error-handling ()
    (with-wcs-math-error-handling (ret (log1+ x))
      (check-wcs-math-result))))

(defun |log2| (x)                       ; C99
  (coercef x 'double-float)
  (with-log-error-handling ()
    (with-wcs-math-error-handling (ret (log x (float 2 x)))
      (check-wcs-math-result))))

(defun |logb| (x)                       ; C99
  (coercef x 'double-float)
  ;; For SBCL: requires it for avoiding `simple-error' and `floating-point-invalid-operation'.
  ;; Allegro CL 10.0 on MacOS raises `simple-error' without it for Inf of NaN.
  (cond ((or (float-nan-p x)
             (float-infinity-p x))
         x)
        (t
         (let ((expon (nth-value 1 (decode-float x))))
           (float (1- expon) x)))))

(defun modf* (x)   ; FIXME: how to treat pointer? (cons as a storage?)
  (coercef x 'double-float)
  (handler-case
      (with-wcs-math-error-handling ((quot rem) (ftruncate x))
        (check-wcs-math-result)
        (values rem quot))
    #+allegro
    (simple-error (e)
      ;; Allegro CL 10.0 on MacOS comes here for Inf of NaN.
      (cond ((float-nan-p x)
             (values x x))
            ((float-infinity-p x)
             (values (float-sign x 0d0) x))
            (t (error e))))))

(defun |scalbn| (x exp)                 ; C99
  (coercef x 'double-float)
  (handler-case
      (with-wcs-math-error-handling (ret (scale-float x exp))
        (check-wcs-math-result)
        (cond
          ;; Allegro CL 10.0 on MacOSX does not cause underflow.
          ;; These checks are required.
          ((and (zerop ret)
                (not (zerop x)))
           (wcs-raise-fe-exception FE_UNDERFLOW)
           ret)
          (t ret)))
    #+sbcl
    (type-error (e)
      ;; For SBCL: If overflow or underflow, SBCL raises `type-error'.
      (cond ((or (float-nan-p x)
                 (float-infinity-p x))
             x)
            ((not (typep exp '(UNSIGNED-BYTE 11)))
             ;; This is not correct.. FIXME!
             (let ((abs-x (abs x)))
               (cond ((or (and (> abs-x 1) (plusp exp))
                          (and (< abs-x 1) (minusp exp)))
                      (wcs-raise-fe-exception FE_OVERFLOW)
                      (float-sign x double-float-positive-infinity))
                     ((or (and (< abs-x 1) (plusp exp))
                          (and (> abs-x 1) (minusp exp)))
                      (wcs-raise-fe-exception FE_UNDERFLOW)
                      (float-sign x 0d0))
                     ((minusp x)        ; == -1
                      (if (evenp exp) 1 -1))
                     (t 1))))
            (t (error e))))
    #+ (or allegro sbcl)
    (simple-error (e)
      ;; Allegro CL 10.0 on MacOS comes here for Inf of NaN.
      ;; For SBCL: It says: Can't decode NaN or infinity: #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY.
      (cond ((or (float-nan-p x)
                 (float-infinity-p x))
             x)
            (t (error e))))))

(defun |scalbln| (x exp)                ; C99
  (|scalbn| x exp))                     ; Because we have BigNum.

;;; Power and absolute-value 

(defun |cbrt| (x)                       ; C99
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (expt x 1/3))
    (cond ((complexp ret)
           (cond ((minusp x)
                  (warn "Current cbrt() implementation returns a principal complex value, defined in ANSI CL, for minus parameters.")
                  ret)
                 (t
                  (wcs-raise-fe-exception FE_INVALID)
                  NAN)))
          (t ret))))

(defun |fabs| (x)
  (coercef x 'double-float)
  (with-wcs-math-error-handling (ret (abs x))
    (check-wcs-math-result)))           ; no error

(defun |hypot| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (handler-case
      (with-wcs-math-error-handling (ret (hypot x y))
        (check-wcs-math-result))
    #+allegro
    (simple-error (e)
      ;; Allegro CL 10.0 on MacOS comes here for NaN or Inf
      (cond ((or (float-infinity-p x)
                 (float-infinity-p y))
             double-float-positive-infinity)
            ((float-nan-p x) x)
            ((float-nan-p y) y)
            (t (error e))))))

;;; pow()

(defun pow-result-significantly-complex-p (ret x y)
  (declare (type double-float x y)
           (type (complex double-float) ret))
  (let* ((realpart (realpart ret))
         (scaled-epsilon
           (* 2 double-float-epsilon (max (abs x) (abs y) (abs realpart)))))
    (> (imagpart ret) scaled-epsilon)))

(defun |pow| (x y &aux (original-y y))
  (coercef x 'double-float)
  (coercef y 'double-float)
  (handler-case
      (with-wcs-math-error-handling (ret (expt x y))
        (cond
          ((complexp ret)
           (let ((realpart (realpart ret)))
             (cond
               ((float-nan-p realpart)
                ;; (expt -1 double-float-negative-infinity) comes here, on Allegro CL 10.1 on MacOS X
                (wcs-raise-fe-exception FE_INVALID)
                realpart)
               ((float-infinity-p realpart)
                (when (and (not (float-infinity-p x))
                           (not (float-infinity-p y)))
                  (wcs-raise-fe-exception FE_OVERFLOW))
                realpart)
               ((pow-result-significantly-complex-p ret x y)
                (wcs-raise-fe-exception FE_INVALID)
                NAN)
               (t
                ;; Allegro CL comes here, even if (plusp y).
                ;;   (expt -1.1 2.0) -> #C(1.2100000000000002d0 -2.963645253936595d-16)
                ;; FIXME: I am suspicious about this routine.
                ;;   (Can I utilize `floating-point:relative-error' ?)
                realpart))))
          ((float-nan-p ret) ; For SBCL; This check is required before `cl:='.
           (cond ((or (float-nan-p x) (float-nan-p y))
                  ret)
                 ((and (= 1 (abs x)) (float-infinity-p y))
                  ;; For SBCL: (expt 1 double-float-positive-infinity) will raise `floating-point-invalid-operation'.
                  (float 1d0 x))
                 (t ret)))
          ;; For SBCL: (expt least-positive-double-float 810d0) does
          ;; not signals `floating-point-underflow'
          ((and (zerop ret)
                (not (zerop x)))
           (wcs-raise-fe-exception FE_UNDERFLOW)
           ret)
          ;; For CCL: It returns the sign of x as-is.
          ((and (zerop ret) (zerop x))
           (if (and (integerp original-y) (oddp original-y))
               ret
               (float 0 ret)))
          (t
           ret)))
    #+ccl
    (type-error (e)
      ;; CCL on MacOSX comes here -- (EXPT -0.0D0 double-float-positive-infinity).
      (cond ((and (zerop x)
                  (float-infinity-p y))
             double-float-positive-infinity)
            (t (error e))))
    #+allegro
    (simple-error (e)
      ;; Allegro CL 10.0 on MacOSX comes here -- (expt 0.0 -1).
      (cond ((and (zerop x)
                  (minusp y))
             (wcs-raise-fe-exception FE_DIVBYZERO :errno ERANGE)
             double-float-positive-infinity)
            (t (error e))))))

(defun |sqrt| (x)
  (coercef x 'double-float)
  (handler-case
      (with-wcs-math-error-handling (ret (sqrt x))
        (check-wcs-math-result))
    #+allegro
    (simple-error (e)
      ;; Allegro CL 10.0 on MacOS comes here; (sqrt NAN).
      (cond ((float-nan-p x) x)
            (t (error e))))))

;;; Error and gamma

;;; TODO: 'erf()', 'erfc()', 'tgamma()', 'lgamma()'

;;; Nearest Integer

(defmacro with-nearest-int-error-handling ((x) &body body)
  (with-gensyms (block-name)
    `(block ,block-name
       ;; For SBCL: raises `floating-point-invalid-operation' for
       ;; (fceiling DOUBLE-FLOAT-POSITIVE-INFINITY)
       (when (or (float-nan-p ,x)
                 (float-infinity-p ,x))
         (return-from ,block-name ,x))
       (when (zerop ,x)                 ; for CCL
         (return-from ,block-name ,x))
       (handler-bind
           ((simple-error
              (lambda (condition) 
                ;; Allegro CL 10.0 on MacOS comes here; (fceiling NAN).
                (cond ((or (float-nan-p ,x)
                           (float-infinity-p ,x))
                       (return-from ,block-name ,x))
                      (t condition)))))
         ,@body))))

(defun |ceil| (x)
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (with-wcs-math-error-handling (ret (fceiling x))
      (check-wcs-math-result))))

(defun |floor| (x)
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (with-wcs-math-error-handling (ret (ffloor x))
      (check-wcs-math-result))))

;;; TODO: 'nearbyint()'
;;; TODO: 'rint()', 'lrint()', 'llrint()'

(defun |round| (x)                      ; C99
  (coercef x 'double-float)
  (with-nearest-int-error-handling (x)
    (with-wcs-math-error-handling (ret (fround x))
      (check-wcs-math-result))))

(defun |lround| (x)                     ; C99
  (coercef x 'double-float)
  ;; FIXME
  (handler-bind
      ((simple-error
         (lambda (condition) 
           ;; Allegro CL 10.0 on MacOS comes here; (fceiling NAN).
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
    (with-wcs-math-error-handling (ret (ftruncate x))
      (check-wcs-math-result))))

;;; Remainder

(defmacro with-mod-family-error-handling ((x y) &body body)
  (declare (ignorable x y))
  `(handler-case
       (cond
         ((float-nan-p ,x) ,x)
         ((float-infinity-p ,x)         ; for CCL.
          (wcs-raise-fe-exception FE_INVALID)
          NAN)
         (t
          (progn ,@body)))
     #+ (or sbcl allegro) 
     (simple-error (e)
       ;; Allegro CL 10.0 on MacOS comes here; (sqrt NAN).
       (cond ((float-nan-p ,x) ,x)
             ((float-nan-p ,y) ,y)
             ((or (float-infinity-p ,x)
                  (zerop ,y))
              (wcs-raise-fe-exception FE_INVALID)
              NAN)
             (t (error e))))))

(defun |fmod| (x y)
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-error-handling (x y)
    (with-wcs-math-error-handling ((quot rem) (ftruncate x y))
      (check-wcs-math-result)
      rem)))

(defun |remainder| (x y)                ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-error-handling (x y)
    (with-wcs-math-error-handling ((quot rem) (fround x y))
      (check-wcs-math-result)
      rem)))

(defun remquo* (x y)                    ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-mod-family-error-handling (x y)
    (with-wcs-math-error-handling ((quotient remainder) (round x y))
      (check-wcs-math-result)
      (values remainder quotient))))

;;; TODO: real 'remquo' -- support pointer passing..

;;; Manipulation

(defun |copysign| (abs sign)            ; C99
  (coercef abs 'double-float)
  (coercef sign 'double-float)
  (float-sign sign abs))

(defun |nan| (&optional (tagp ""))      ; C99
  (cond
    ((string= tagp "") NAN)
    (t
     ;; Uses TAGP for specifing lower bits of NAN.
     ;; This is my experiment.
     (let* ((lower-bits-int (or (parse-integer tagp :junk-allowed t)
                                0))
            (bitmask #b11111111111111111111111) ; 23 bits.
            (nan-bits (double-float-bits NAN))
            (new-nan-bits (logior (logand lower-bits-int bitmask)
                                  nan-bits)))
       (bits-double-float new-nan-bits)))))

;;; nextafter() (C99)

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

(defun check-nextafter-result (ret x)
  (cond ((and (float-infinity-p ret)
              (not (float-infinity-p x)))
         (wcs-raise-fe-exception FE_OVERFLOW))
        ((or (zerop ret)
             (float-subnormal-p ret))
         (wcs-raise-fe-exception FE_UNDERFLOW))))

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
       (check-nextafter-result ret x)
       ret))))

(defun |nexttoward| (x y &aux (long-float-x (coerce x 'long-float)))
  (coercef x 'double-float)
  (coercef y 'long-float)
  (cond
    ((float-nan-p x) x)
    ((float-nan-p y) y)
    ((= long-float-x y) (float y x))
    (t
     (let ((ret (if (< long-float-x y)
                    (step-double-float-plus x)
                    (step-double-float-minus x))))
       (check-nextafter-result ret x)
       ret))))

;;; Maximum, minimum, and positive difference

(defun |fdim| (x y)                     ; C99
  (coercef x 'double-float)
  (coercef y 'double-float)
  (with-wcs-math-error-handling (diff (- x y))
    (check-wcs-math-result)
    (if (minusp diff)
        (float 0 x)
        diff)))

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

;;; Floating multiply-add

;;; TODO: fma()

;;; Comparison

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
