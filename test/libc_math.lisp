(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(defmacro is.float-equal (x y)
  `(is (float-equal ,x ,y)))

(defmacro is.complexp (x)
  `(is (complexp ,x)))

(defmacro is.float-nan-p (x &optional suppress-all-error)
  `(handler-case (is (float-nan-p ,x))
     (arithmetic-error () t)
     ,@(if suppress-all-error
           '((error (e)
              (warn "caught unexpected error ~A; ~A"
               (type-of e) e))))))

(defmacro may-fail (form)
  (let ((result (gensym)) (condition (gensym)))
    `(multiple-value-bind (,result ,condition)
         (ignore-errors ,form)
       (or ,result
           (if ,condition
               (warn "~A raised ~A" ',form ,condition)
               (warn "~A was evaluated to false" ',form))))))

(test test-math-fabs
  #{
  is (fabs (0.0) == 0.0);
  is.float-equal (fabs (5.0), 5.0);
  is.float-equal (fabs (-5.0), 5.0);
  
  // ; Specials
  // ; I use '==' (which is `eql'), because (float-equal +Inf +Inf) is false.
  is (fabs (double-float-negative-infinity) == double-float-positive-infinity);
  }#)

(test test-math-fmod
  #{
  is.float-equal (fmod(3.123, 2.0), 1.123);
  is.float-equal (fmod(-3.123, 2.0), -1.123);
  is.float-equal (fmod(3.123, -2.0), 1.123);
  is.float-equal (fmod(-3.123, -2.0), -1.123);
  
  // ; Specials
  is (fmod(0.0, 2.0) == 0.0);
  // ; Sadly, SBCL 1.5.0 on MacOS X does not preserve minus-zero sign.
  may-fail (fmod(-0.0, 2.0) == -0.0);
  is.float-equal (fmod(-0.0, 2.0), -0.0);
  is.float-nan-p (fmod(99.0, 0.0)); // Domain error.
  is.float-nan-p (fmod(double-float-positive-infinity, 10.0)); // Domain error.
  }#)

(test test-math-remainder
  #{
  is.float-equal (remainder(3.125, 2.0), -0.875);
  is.float-equal (remainder(-3.125, 2.0), 0.875);
  is.float-equal (remainder(3.125, -2.0), -0.875);
  is.float-equal (remainder(-3.125, -2.0), 0.875);
  
  // ; Specials
  is (remainder(0.0, 2.0) == 0.0);
  // ; Sadly, SBCL 1.5.0 on MacOS X does not preserve minus-zero sign.
  may-fail (remainder(-0.0, 2.0) == -0.0);
  is.float-equal (remainder(-0.0, 2.0), -0.0);
  is.float-nan-p (remainder(99.0, 0.0), t); // Domain error.
  is.float-nan-p (remainder(double-float-positive-infinity, 10.0), t); // Domain error.
  }#)

(test test-math-fmax
  #{
  is (fmax(3.125, 2.0) == 3.125);
  is (fmax(double-float-negative-infinity, 2.0) == 2.0);
  is (fmax(double-float-positive-infinity, 2.0) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-fmin
  #{
  is (fmin(3.125, 2.0) == 2.0);
  is (fmin(double-float-negative-infinity, 2.0) == double-float-negative-infinity);
  is (fmin(double-float-positive-infinity, 2.0) == 2.0);
  // ; TODO: add NaN test.
  }#)

(test test-math-exp
  #{
  is (`(< 2.71828
         #{ return exp(1); }#
         2.71829));
  is.float-equal (exp(0), 1);
  is (exp(double-float-negative-infinity) == 0.0);
  is (exp(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-exp2
  #{
  is.float-equal (exp2(1.0), 2);
  is (exp2(0.0) == 1.0);
  is (exp2(double-float-negative-infinity) == 0.0);
  is (exp2(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-expm1
  #{
  is (`(< 1.71828
         #{ return expm1(1); }#
         1.71829));
  is (expm1(0.0) == 0.0);
  is.float-equal (expm1(double-float-negative-infinity), -1.0);
  may-fail ( is (expm1(double-float-positive-infinity) == double-float-positive-infinity));
  // ; TODO: add NaN test.
  }#)

(test test-math-log
  #{
  is.float-equal (log (exp (2)), 2);
  is (log(1.0) == 0.0);
  is (log(0.0) == double-float-negative-infinity);
  is.complexp (log(-1.0)); // FIXME: Common Lisp returns a complex.
  is (log(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-log10
  #{
  is.float-equal (log10 (100), 2);
  is (log10 (1.0) == 0.0);
  is (log10 (0.0) == double-float-negative-infinity);
  is.complexp (log10(-1.0)); // FIXME: Common Lisp returns a complex.
  is (log10(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-log2
  #{
  is.float-equal (log2 (8), 3);
  is (log2 (1.0) == 0.0);
  is (log2 (0.0) == double-float-negative-infinity);
  is.complexp (log10(-1.0)); // FIXME: Common Lisp returns a complex.
  is (log2(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-log1p
  #{
  is.float-equal (log1p (11), log (12));
  is (log1p (0.0) == 0.0);
  is (log1p (-1.0) == double-float-negative-infinity);
  is.complexp (log1p (-2)); // FIXME: Common Lisp returns a complex.
  is.complexp (log1p (double-float-negative-infinity)); // FIXME: Common Lisp returns a complex.
  is (log2(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-pow
  #{
  is.float-equal (pow (2, 3), 8.0);
  is.float-equal (pow (-1.1, 2), 1.21);
  is.float-equal (pow (-1.1, -2), `(/ 1.21));
  // ; Specials
  is (pow (0.0, -1) == double-float-positive-infinity);
  is (pow (-0.0, -1) == double-float-negative-infinity);
  is (pow (0.0, -2) == double-float-positive-infinity);
  is (pow (-0.0, -2.5) == double-float-positive-infinity);
  is (pow (-0.0, double-float-negative-infinity) == double-float-positive-infinity);
  is (pow (0.0, 1) == 0.0);
  is (pow (-0.0, 1) == -0.0);
  is (pow (0.0, 2) == 0.0);
  is (pow (-0.0, 2.5) == 0.0);
  may-fail (pow (-1, double-float-positive-infinity) == 1.0);
  may-fail (pow (-1, double-float-negative-infinity) == 1.0);
  may-fail (pow (1, double-float-positive-infinity) == 1.0);
  may-fail (pow (1, double-float-negative-infinity) == 1.0);
  // ; TODO: add pow(1, NaN) test.
  is (pow (double-float-positive-infinity, +0.0) == 1.0);
  is (pow (double-float-negative-infinity, -0.0) == 1.0);
  // ; TODO: add pow(NaN, 0)
  is.complexp (pow (-2.1, 0.3)); // FIXME: Common Lisp returns a complex.
  is (pow (least-positive-double-float, double-float-negative-infinity)
          == double-float-positive-infinity);
  is (pow (double-float-positive-infinity, double-float-negative-infinity)
          == 0.0);
  is (pow (least-negative-double-float, double-float-positive-infinity)
          == 0.0);
  is (pow (double-float-negative-infinity, double-float-positive-infinity)
          == double-float-positive-infinity);
  is (pow (double-float-negative-infinity, -3) == -0.0);
  is (pow (double-float-negative-infinity, -2) == 0.0);
  is (pow (double-float-negative-infinity, -2.1) == 0.0);
  is (pow (double-float-negative-infinity, 3) == double-float-negative-infinity);
  is (pow (double-float-negative-infinity, 2) == double-float-positive-infinity);
  is (pow (double-float-negative-infinity, 2.1) == double-float-positive-infinity);
  is (pow (double-float-positive-infinity, -10) == 0.0);
  is (pow (double-float-positive-infinity, double-float-negative-infinity) == 0.0);
  is (pow (double-float-positive-infinity, 10) == double-float-positive-infinity);
  is (pow (double-float-positive-infinity, double-float-positive-infinity)
          == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-sqrt
  #{
  is.float-equal (sqrt (100.0), 10.0);
  // ; Specials
  is.complexp (sqrt (-2.1)); // FIXME: Common Lisp returns a complex.
  is (sqrt (double-float-positive-infinity) == double-float-positive-infinity);
  is (sqrt (0.0) == 0.0);
  is (sqrt (-0.0) == -0.0);
  // ; TODO: add NaN test.
  }#)

(test test-math-cbrt
  #{
  is.float-equal (cbrt (1000.0), 10.0);
  // ; Specials
  is.complexp (cbrt (-2.1)); // FIXME: Common Lisp returns a complex.
  is (cbrt (double-float-positive-infinity) == double-float-positive-infinity);
  may-fail (cbrt (double-float-negative-infinity) == double-float-negative-infinity);
  is (cbrt (0.0) == 0.0);
  may-fail (cbrt (-0.0) == -0.0);
  // ; TODO: add NaN test.
  }#)

(test test-math-hypot
  #{
  is.float-equal (hypot (1, 1), 1.41421356);
  is.float-equal (hypot (3, 4), 5);
  is.float-equal (hypot (1.23, -4.56), hypot (4.56, -1.23));
  is.float-equal (hypot (1.23, 0), fabs (1.23));
  is.float-equal (hypot (-0, -9928.123456), fabs (-9928.123456));
  is (hypot (double-float-negative-infinity, 0) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-sin
  #{
  is.float-equal (sin (0.0), 0.0);
  is.float-equal (sin (PI / 6), 0.5);
  is.float-equal (sin (-PI / 3), -sqrt (3) / 2);
  is.float-equal (sin (PI / 2), 1.0);
  // ; Specials
  is (sin (-0.0) == -0.0);
  is.float-nan-p (sin (double-float-positive-infinity));
  is.float-nan-p (sin (double-float-negative-infinity));
  // ; TODO: add NaN test.
  }#)

(test test-math-cos
  #{
  is.float-equal (cos (0.0), 1.0);
  is.float-equal (cos (PI / 6),  sqrt (3) / 2);
  is.float-equal (cos (-PI / 3), 0.5);
  is.float-equal (cos (PI / 2), 0);
  // ; Specials
  is (cos (-0.0) == 1.0);
  is.float-nan-p (cos (double-float-positive-infinity));
  is.float-nan-p (cos (double-float-negative-infinity));
  // ; TODO: add NaN test.
  }#)

(test test-math-tan
  #{
  is.float-equal (tan (0.0), 0.0);
  is.float-equal (tan (PI / 6),  1 / sqrt (3));
  is.float-equal (tan (-PI / 3), -sqrt (3));
  may-fail (tan (PI / 2));
  // ; Specials
  is (tan (-0.0) == -0.0);
  is.float-nan-p (tan (double-float-positive-infinity));
  is.float-nan-p (tan (double-float-negative-infinity));
  // ; TODO: add NaN test.
  }#)

(test test-math-asin
  #{
  is.float-equal (asin (0.0), 0.0);
  is.float-equal (asin (0.5), PI / 6);
  is.float-equal (asin (-sqrt (3) / 2), -PI / 3);
  is.float-equal (asin (1.0), PI / 2);
  // ; Specials
  is (asin (-0.0) == -0.0);
  is.complexp (asin (1.001)); // FIXME: Common Lisp returns a complex.
  is.complexp (asin (double-float-negative-infinity)); // FIXME: Common Lisp returns a complex.
  // ; TODO: add NaN test.
  }#)

(test test-math-acos
  #{
  is.float-equal (acos (1.0), 0.0);
  is.float-equal (acos (sqrt (3) / 2), PI / 6);
  is.float-equal (acos (0.5), PI / 3);
  is.float-equal (acos (0), PI / 2);
  // ; Specials
  is.complexp (acos (1.0001));
  is.complexp (acos (double-float-negative-infinity));
  // ; TODO: add NaN test.
  }#)

(test test-math-atan
  #{
  is.float-equal (atan (0.0), 0.0);
  is.float-equal (atan (1 / sqrt (3)), PI / 6);
  is.float-equal (atan (-sqrt (3)), -PI / 3);
  // ; Specials
  is (atan (-0.0) == -0.0);
  is.float-equal (atan (double-float-positive-infinity), PI / 2);
  is.float-equal (atan (double-float-negative-infinity), -PI / 2);
  // ; TODO: add NaN test.
  }#)

(test test-math-atan2
  #{
  is.float-equal (atan2 (0.0, 1), 0.0);
  is.float-equal (atan2 (1, sqrt (3)), PI / 6);
  is.float-equal (atan2 (-1, 1 / sqrt (3)), -PI / 3);
  // ; Specials
  is (atan2 (0.0, -0.0) == PI);
  is (atan2 (-0.0, -0.0) == -PI);
  is (atan2 (0.0, 0.0) == 0.0);
  is (atan2 (-0.0, 0.0) == -0.0);
  is (atan2 (double-float-positive-infinity, 10) == PI / 2);
  is (atan2 (double-float-negative-infinity, -100) == -PI / 2);
  is (atan2 (double-float-positive-infinity, double-float-negative-infinity) == PI * 3 / 4);
  is (atan2 (double-float-negative-infinity, double-float-positive-infinity) == -PI / 4);
  is (atan2 (-9999, -0.0) == - PI / 2);
  is (atan2 (9999, -0.0) == PI / 2);
  is (atan2 (9999, double-float-negative-infinity) == PI);
  is (atan2 (-9999, double-float-negative-infinity) == - PI);
  is (atan2 (9999, double-float-positive-infinity) == 0.0);
  is (atan2 (-9999, double-float-positive-infinity) == -0.0);
  // ; TODO: add NaN test.
  }#)

(test test-math-sinh
  #{
  is.float-equal (sinh (2.3), (exp (2.3) - exp (-2.3)) / 2);
  is.float-equal (sinh (-9), (exp (-9) - exp (- - 9)) / 2);
  is (sinh (1000.0) == double-float-positive-infinity);
  // ; Specials
  is (sinh (0.0) == 0.0);
  is (sinh (-0.0) == -0.0);
  is (sinh (double-float-positive-infinity) == double-float-positive-infinity);
  is (sinh (double-float-negative-infinity) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-cosh
  #{
  is.float-equal (cosh (2.3), (exp (2.3) + exp (-2.3)) / 2);
  is.float-equal (cosh (-9), (exp (-9) + exp (- - 9)) / 2);
  is (cosh (1000.0) == double-float-positive-infinity);
  // ; Specials
  is (cosh (0.0) == 1.0);
  is (cosh (-0.0) == 1.0);
  is (cosh (double-float-positive-infinity) == double-float-positive-infinity);
  is (cosh (double-float-negative-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-tanh
  #{
  is.float-equal (tanh (2.3), (exp (2.3) - exp (-2.3)) / (exp (2.3) + exp (-2.3)));
  is.float-equal (tanh (-9), (exp (-9) - exp (- - 9)) / (exp (-9) + exp (- - 9)));
  // ; Specials
  is (tanh (0.0) == 0.0);
  is (tanh (-0.0) == -0.0);
  is (tanh (double-float-positive-infinity) == 1.0);
  is (tanh (double-float-negative-infinity) == -1.0);
  // ; TODO: add NaN test.
  }#)

(test test-math-asinh
  #{
  is.float-equal (asinh (sinh (10.0)), 10.0);
  is.float-equal (asinh (sinh (-1.0)), -1.0);
  // ; Specials
  is (asinh (0.0) == 0.0);
  is (asinh (-0.0) == -0.0);
  is (asinh (double-float-positive-infinity) == double-float-positive-infinity);
  is (asinh (double-float-negative-infinity) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-acosh
  #{
  is.float-equal (acosh (cosh (10.0)), 10.0);
  is.float-equal (acosh (cosh (1.1)), 1.1);
  // ; Specials
  is.complexp (acosh (0.9));
  is (acosh (1.0) == 0.0);
  is (acosh (double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-atanh
  #{
  is.float-equal (atanh (tanh (.9)), .9);
  is.float-equal (atanh (tanh (-.1)), -.1);
  // ; Specials
  is (atanh (0.0) == 0.0);
  is (atanh (-0.0) == -0.0);
  is (atanh (1.0) == double-float-positive-infinity);
  is (atanh (-1.0) == double-float-negative-infinity);
  is.complexp (atanh (1.1));
  is.complexp (atanh (-1.1));
  // ; TODO: add NaN test.
  }#)
