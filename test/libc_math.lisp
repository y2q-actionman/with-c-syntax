(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(defmacro check-errno (form expected-errno)
  `(let ((|errno| nil))
     (prog1 ,form
       (is (eql |errno| ,expected-errno)))))

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

(test test-math-exp
  #{
  is (`(< 2.71828
         #{ return |exp|(1.0d0); }#
         2.71829));
  // ; TODO: I require implicit numeric type conversions to write it like exp(0);
  is.float-equal (|exp|(0.0d0), 1.0d0);
  check-errno (is(|exp|(most-positive-double-float) == HUGE_VAL), ERANGE);
  check-errno (is(|exp|(most-negative-double-float) == 0.0), ERANGE);
  check-errno (is(|exp|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is(|exp|(double-float-negative-infinity) == 0.0), nil);
  is (float-nan-p (|exp|(double-float-nan)));
  }#)

(test test-math-exp2
  #{
  is.float-equal (|exp2|(2.0), 4.0);
  is (|exp2|(0.0) == 1.0);
  check-errno (is (|exp2|(most-positive-double-float) == HUGE_VAL), ERANGE);
  check-errno (is (|exp2|(most-negative-double-float) == 0.0), ERANGE);
  check-errno (is (|exp2|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is (|exp2|(double-float-negative-infinity) == 0.0), nil);
  is (float-nan-p (|exp2|(double-float-nan)));
  }#)

(test test-math-expm1
  #{
  is (`(< 1.71828
         #{ return |expm1|(1.0d0); }#
         1.71829));
  is (|expm1|(0.0) == 0.0);
  is.float-equal (|expm1|(double-float-negative-infinity), -1.0);
  check-errno (is (|expm1|(most-positive-double-float) == HUGE_VAL), ERANGE);
  check-errno (is (|expm1|(most-negative-double-float) == -1.0), ERANGE);
  check-errno (is (|expm1|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is (|expm1|(double-float-negative-infinity) == -1.0), nil);
  is (float-nan-p (|expm1|(double-float-nan)));
  }#)

(test test-math-log
  #{
  is.float-equal (|log| (|exp| (2.0)), 2);
  check-errno (is (|log|(1.0) == 0.0), nil);
  check-errno (is (|log|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log|(-1.0) == double-float-nan), EDOM);
  check-errno (is (|log|(-HUGE_VAL) == double-float-nan), EDOM);
  check-errno (is (|log|(double-float-nan) == double-float-nan), nil);
  }#)

(test test-math-log10
  #{
  is.float-equal (|log10| (100.0), 2);
  check-errno (is (|log10|(1.0) == 0.0), nil);
  check-errno (is (|log10|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log10|(-1.0) == double-float-nan), EDOM);
  check-errno (is (|log10|(-HUGE_VAL) == double-float-nan), EDOM);
  check-errno (is (|log10|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log10|(double-float-nan) == double-float-nan), nil);
  }#)

(test test-math-log1p
  #{
  is.float-equal (|log1p| (11.0), |log| (12.0));
  check-errno (is (|log1p|(0.0) == 0.0), nil);
  is (|log1p|(-0.5) < 0);
  check-errno (is (|log1p|(-1.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log1p|(-2.0)) == double-float-nan, EDOM);
  check-errno (is (|log1p|(-HUGE_VAL) == double-float-nan), EDOM);
  check-errno (is (|log1p|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log1p|(double-float-nan) == double-float-nan), nil);
  }#)

(test test-math-log2
  #{
  is.float-equal (|log2| (8.0), 3);
  check-errno (is (|log2|(1.0) == 0.0), nil);
  check-errno (is (|log2|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log2|(-1.0) == double-float-nan), EDOM);
  check-errno (is (|log2|(-HUGE_VAL) == double-float-nan), EDOM);
  check-errno (is (|log2|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log2|(double-float-nan) == double-float-nan), nil);
  }#)

(test test-math-cbrt
  #{
  double tmp;
  
  is.float-equal (|cbrt| (1000.0), 10.0);
  
  // ; FIXME: cbrt(-1.0) may return a complex. Under implementation...
  check-errno (tmp = |cbrt| (-1.0), nil);
  is (tmp == -1.0 \|\| complexp (tmp));
  
  is (|cbrt| (0.0) == 0.0);
  is (|cbrt| (-0.0) == -0.0);
  check-errno (is (|cbrt| (double-float-positive-infinity) == double-float-positive-infinity),
                  nil);
  is (|cbrt| (double-float-negative-infinity) == double-float-negative-infinity);
  is (|cbrt| (double-float-nan) == double-float-nan);
  }#)

(test test-math-hypot
  #{
  is.float-equal (hypot (1, 1), 1.414213562373095145474621858739);
  is.float-equal (hypot (3, 4), 5);
  is.float-equal (hypot (1.23, -4.56), hypot (4.56, -1.23));
  is.float-equal (hypot (1.23, 0), |fabs| (1.23));
  is.float-equal (hypot (-0, -9928.123456), |fabs| (-9928.123456));

  check-errno (is (hypot (most-positive-double-float, most-positive-double-float)
                         == double-float-positive-infinity),
                  ERANGE);
  is (hypot (least-positive-double-float, least-positive-double-float) > 0);
  
  is (hypot (double-float-negative-infinity, 0) == double-float-positive-infinity);
  is (hypot (0, double-float-positive-infinity) == double-float-positive-infinity);
  is (hypot (double-float-nan, double-float-positive-infinity) == double-float-positive-infinity);
  is (hypot (-1, double-float-nan) == double-float-nan);
  }#)

(test test-math-fabs
  #{
  is (|fabs|(0.0) == 0.0);
  is.float-equal (|fabs|(5.0), 5.0);
  is.float-equal (|fabs|(-5.0), 5.0);
  
  // ; Specials
  // ; I use '==' (which is `eql'), because (float-equal +Inf +Inf) is false.
  is (|fabs|(double-float-negative-infinity) == double-float-positive-infinity);
  is (|fabs|(double-float-positive-infinity) == double-float-positive-infinity);
  is (float-nan-p (|fabs|(double-float-nan)));
  }#)

(test test-math-fmod
  #{
  is.float-equal (|fmod|(3.123, 2.0), 1.123);
  is.float-equal (|fmod|(-3.123, 2.0), -1.123);
  is.float-equal (|fmod|(3.123, -2.0), 1.123);
  is.float-equal (|fmod|(-3.123, -2.0), -1.123);
  
  // ; Specials
  is (|fmod|(0.0, 2.0) == 0.0);
  is (|fmod|(-0.0, 2.0) == -0.0);
  is.float-equal (|fmod|(-0.0, 2.0), -0.0);
  check-errno (is (float-nan-p (|fmod|(99.0, 0.0))), EDOM);
  check-errno (is (float-nan-p (|fmod|(double-float-positive-infinity, 10.0))), EDOM);
  check-errno (is (float-nan-p (|fmod|(double-float-nan, 1.0))), nil);
  check-errno (is (float-nan-p (|fmod|(1.0, double-float-nan))), nil);
  }#)

(test test-math-remainder
  #{
  is.float-equal (|remainder|(3.125, 2.0), -0.875);
  is.float-equal (|remainder|(-3.125, 2.0), 0.875);
  is.float-equal (|remainder|(3.125, -2.0), -0.875);
  is.float-equal (|remainder|(-3.125, -2.0), 0.875);
  
  // ; Specials
  is (|remainder|(0.0, 2.0) == 0.0);
  is (|remainder|(-0.0, 2.0) == -0.0);
  is.float-equal (|remainder|(-0.0, 2.0), -0.0);
  check-errno (is (float-nan-p (|remainder|(99.0, 0.0))), EDOM);
  check-errno (is (float-nan-p (|remainder|(double-float-positive-infinity, 10.0))),
                  EDOM);
  check-errno (is (float-nan-p (|remainder|(double-float-nan, 1.0))), nil);
  check-errno (is (float-nan-p (|remainder|(1.0, double-float-nan))), nil);
  }#)

(test test-math-remquo*
  ;; This test shows how to treat multiple values in with-c-syntax.
  #{
  double rem, quo;

  values (rem, quo) = remquo* (3.125, 2.0);
  is (float-equal (rem, -0.875));
  is (quo == 2);

  values (rem, quo) = remquo* (-3.125, 2.0);
  is (float-equal (rem, 0.875));
  is (quo == -2);

  values (rem, quo) = remquo* (3.125, -2.0);
  is (float-equal (rem, -0.875));
  is (quo == -2);

  values (rem, quo) = remquo* (-3.125, -2.0);
  is (float-equal (rem, 0.875));
  is (quo == 2);

  // ; Specials
  check-errno (is (float-nan-p (remquo* (double-float-positive-infinity, 2.0))), EDOM);
  check-errno (is (float-nan-p (remquo* (double-float-negative-infinity, 2.0))), EDOM);
  check-errno (is (float-nan-p (remquo* (10.0, +0.0))), EDOM);
  check-errno (is (float-nan-p (remquo* (999.0, -0.0))), EDOM);
  check-errno (is (float-nan-p (remquo* (double-float-nan, +1.0))), nil);
  check-errno (is (float-nan-p (remquo* (1.0, double-float-nan))), nil);
  }#)

;;; FIXME: I have |NAN| and |nan| in Libc package. So this code misses which to use.
#+ ()
(test test-math-nan
  #{
  is.float-nan-p (|nan|(""));
  }#)

;; This test assumes double-float is the Binary64 of IEEE-754.
;; TODO: Make C hexadecimal float notation to main line, to cleaning up.
(test test-math-nextafter
  #{
  double before-1d0 = `#2{ 0x1.fffffffffffffp-1; }#;
  double next-of-least-positive-double-float =  `#2{ 0x1p-1073; }#;
  double most-positive-subnormal-double-float =  `#2{ 0x1.ffffffffffffep-1023; }#;
  
  `(muffle-unused-code-warning
     (unless (= least-positive-double-float #2{ 0x1p-1074; }#
                )
       (warn "Your double-float representation is not expected one of this test.")));

  // ; Simple case
  is (|nextafter|(1.5d0,  10d0) == `#2{ 0x1.8000000000001p+0; }#
                 );
  is (|nextafter|(1.5d0, -10d0) == `#2{ 0x1.7ffffffffffffp+0; }#
                 );
  is (|nextafter|(1.0d0,  10d0) == `#2{ 0x1.0000000000001p+0; }#
                 );

  // ; Carry and borrow
  is (|nextafter|(1.0d0, -10d0) == before-1d0);
  is (|nextafter|(before-1d0, +10d0) == 1.0d0);

  is (|nextafter|(-1.0d0, +10d0) == - before-1d0);
  is (|nextafter|(- before-1d0, -10d0) == -1.0d0);

  // ; Border of normals and subnormals.
  check-errno (is (|nextafter|(least-positive-normalized-double-float, -1d0) == most-positive-subnormal-double-float),
                  ERANGE);
  check-errno (is (|nextafter|(most-positive-subnormal-double-float, +1d0) == least-positive-normalized-double-float),
                  nil);

  check-errno (is (|nextafter|(least-negative-normalized-double-float, +1d0) == - most-positive-subnormal-double-float),
                  ERANGE);
  check-errno (is (|nextafter|(- most-positive-subnormal-double-float, -1d0) == least-negative-normalized-double-float),
                  nil);
  
  // ; Zero and subnormals.
  check-errno (is (|nextafter|(least-negative-double-float, 1d0) == -0d0), ERANGE);
  check-errno (is (|nextafter|(-0d0, 1d0) == least-positive-double-float), ERANGE);
  check-errno (is (|nextafter|(least-positive-double-float, 1d0) == next-of-least-positive-double-float), ERANGE);

  check-errno (is (|nextafter|(next-of-least-positive-double-float, -1d0) == least-positive-double-float), ERANGE);
  check-errno (is (|nextafter|(least-positive-double-float, -1d0) == 0d0), ERANGE);
  check-errno (is (|nextafter|(0d0, -1d0) == least-negative-double-float), ERANGE);
  
  // ; Infinities
  check-errno (is (|nextafter|(most-positive-double-float, double-float-positive-infinity)
                              == double-float-positive-infinity),
                  ERANGE);
  check-errno (is (|nextafter|(double-float-negative-infinity, double-float-positive-infinity)
                              == most-negative-double-float),
                  nil);
  check-errno (is (|nextafter|(most-negative-double-float, double-float-negative-infinity)
                              == double-float-negative-infinity),
                  ERANGE);
  check-errno (is (|nextafter|(double-float-negative-infinity, double-float-negative-infinity)
                              == double-float-negative-infinity),
                  nil);

  // ; NaN
  check-errno (is (float-nan-p (|nextafter|(double-float-nan, double-float-nan))), nil);
  check-errno (is (float-nan-p (|nextafter|(double-float-nan, 1.0))), nil);
  check-errno (is (float-nan-p (|nextafter|(1.0, double-float-nan))), nil);
  }#)

(test test-math-fdim
  #{
  is (|fdim|(2.0, 1.0) == 1.0);
  is (|fdim|(1.0, 2.0) == 0.0);
  is (|fdim|(double-float-positive-infinity, 1.0) == double-float-positive-infinity);
  check-errno (is (|fdim|(most-positive-double-float, most-negative-double-float) == HUGE_VAL),
                  ERANGE);
  check-errno (is (float-nan-p (|fdim|(-1.0, double-float-nan))), nil);
  }#)

(test test-math-fmax
  #{
  is (|fmax|(3.125, 2.0) == 3.125);
  is (|fmax|(double-float-negative-infinity, 2.0) == 2.0);
  is (|fmax|(double-float-positive-infinity, 2.0) == double-float-positive-infinity);
  is (|fmax|(double-float-nan, 1.0) == 1.0);
  is (|fmax|(-1.0, double-float-nan) == -1.0);
  is (float-nan-p (|fmax|(double-float-nan, double-float-nan)));
  }#)

(test test-math-fmin
  #{
  is (|fmin|(3.125, 2.0) == 2.0);
  is (|fmin|(double-float-negative-infinity, 2.0) == double-float-negative-infinity);
  is (|fmin|(double-float-positive-infinity, 2.0) == 2.0);
  is (|fmin|(double-float-nan, 1.0) == 1.0);
  is (|fmin|(-1.0, double-float-nan) == -1.0);
  is (float-nan-p (|fmin|(double-float-nan, double-float-nan)));
  }#)




(test test-math-pow
  #{
  is.float-equal (pow (2, 3), 8.0);
  is.float-equal (pow (-1.1, 2), 1.21);
  is.float-equal (pow (-1.1, -2), `(/ 1.21));
  // ; Specials
  signals (arithmetic-error, pow (0.0, -1));
  may-fail (pow (0.0, -1) == double-float-positive-infinity);
  signals (arithmetic-error, pow (-0.0, -1));
  may-fail (pow (-0.0, -1) == double-float-negative-infinity);
  signals (arithmetic-error, pow (0.0, -2));
  may-fail (pow (0.0, -2) == double-float-positive-infinity);
  signals (arithmetic-error, pow (-0.0, -2.5));
  may-fail (pow (-0.0, -2.5) == double-float-positive-infinity);
  may-fail (pow (-0.0, double-float-negative-infinity) == double-float-positive-infinity);
  is (pow (0.0, 1) == 0.0);
  is (pow (-0.0, 1) == -0.0);
  is (pow (0.0, 2) == 0.0);
  is (pow (-0.0, 2.5) == 0.0);
  may-fail (pow (-1, double-float-positive-infinity) == 1.0);
  may-fail (pow (-1, double-float-negative-infinity) == 1.0);
  may-fail (pow (1, double-float-positive-infinity) == 1.0);
  may-fail (pow (1, double-float-negative-infinity) == 1.0);
  // ; TODO: add pow(1, NaN) test.
  signals (arithmetic-error, pow (double-float-positive-infinity, +0.0) == 1.0);
  signals (arithmetic-error, pow (double-float-negative-infinity, -0.0) == 1.0);
  // ; TODO: add pow(NaN, 0)
  is.complexp (pow (-2.1, 0.3)); // FIXME: Common Lisp returns a complex.
  may-fail (complexp (pow (-2.1, 0.3))); // FIXME: Common Lisp returns a complex.
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
  signals (arithmetic-error, asin (double-float-negative-infinity)); // FIXME: Common Lisp returns a complex.
  may-fail (complexp (asin (double-float-negative-infinity))); // FIXME: Common Lisp returns a complex.
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
  signals(arithmetic-error, acos (double-float-negative-infinity));
  may-fail (complexp (acos (double-float-negative-infinity)));
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
  signals (arithmetic-error, sinh (1000.0));
  may-fail (sinh (1000.0) == double-float-positive-infinity);
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
  signals (arithmetic-error, cosh (1000.0));
  may-fail (cosh (1000.0) == double-float-positive-infinity);
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
  signals (arithmetic-error, atanh (1.0));
  may-fail (atanh (1.0) == double-float-positive-infinity);
  signals (arithmetic-error, atanh (-1.0));
  may-fail (atanh (-1.0) == double-float-negative-infinity);
  is.complexp (atanh (1.1));
  is.complexp (atanh (-1.1));
  // ; TODO: add NaN test.
  }#)

(test test-math-ceil
  #{
  is.float-equal (ceil (3.14), 4.0);
  is.float-equal (ceil (-3.14), -3.0);
  // ; Specials
  is (ceil (0.0) == 0.0);
  is (ceil (-0.0) == -0.0);
  signals (arithmetic-error, ceil (double-float-positive-infinity));
  may-fail (ceil (double-float-positive-infinity) == double-float-positive-infinity);
  signals (arithmetic-error, ceil (double-float-negative-infinity));
  may-fail (ceil (double-float-negative-infinity) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-floor
  #{
  is.float-equal (floor (3.14), 3.0);
  is.float-equal (floor (-3.14), -4.0);
  // ; Specials
  is (floor (0.0) == 0.0);
  is (floor (-0.0) == -0.0);
  // signals (arithmetic-error, floor (double-float-positive-infinity));
  is (floor (double-float-positive-infinity) == double-float-positive-infinity);
  // signals (arithmetic-error, floor (double-float-negative-infinity));
  is (floor (double-float-negative-infinity) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-round
  #{
  is.float-equal (round (3.14), 3.0);
  is.float-equal (round (3.9), 4.0);
  is.float-equal (round (-3.14), -3.0);
  is.float-equal (round (-3.9), -4.0);
  // ; Specials
  is (round (0.0) == 0.0);
  may-fail (round (-0.0) == -0.0);
  may-fail (round (double-float-positive-infinity) == double-float-positive-infinity);
  may-fail (round (double-float-negative-infinity) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-frexp*-and-ldexp
  #{
  double d;
  int e;

  values (d, e) = frexp* (3.14);
  is (0.5 <= d && d < 1);
  is.float-equal (ldexp (d, e), 3.14);
  
  values (d, e) = frexp* (-10.001);
  is (-0.5 >= d && d > -1);
  is.float-equal (ldexp (d, e), -10.001);
  
  // ; Specials (frexp*)
  values (d, e) = frexp* (0.0);
  is (d == 0.0);
  is (e == 0);
  values (d, e) = frexp* (-0.0);
  is (d == -0.0);
  is (e == 0);
  if (may-fail (values (d, e) = frexp* (double-float-positive-infinity))) {
    is (d == double-float-positive-infinity);
  }
  if (may-fail (values (d, e) = frexp* (double-float-negative-infinity))) {
    is (d == double-float-negative-infinity);
  }

  // ; Specials (ldexp)
  is (ldexp (0.0, most-positive-fixnum) == 0.0);
  is (ldexp (-0.0, most-negative-fixnum) == -0.0);
  may-fail (ldexp (double-float-positive-infinity, most-negative-fixnum)
                  == double-float-positive-infinity);
  is (ldexp (double-float-negative-infinity, -1) == double-float-negative-infinity);
  is (ldexp (double-float-negative-infinity, 0) == double-float-negative-infinity);
  
  // ; TODO: add NaN test.
  }#)

(test test-math-modf*
  #{
  double rem, quot;
  
  values (rem, quot) = modf* (1.125);
  is.float-equal (rem, 0.125);
  is.float-equal (quot, 1.0);
  
  values (rem, quot) = modf* (-1.125);
  is.float-equal (rem, -0.125);
  is.float-equal (quot, -1.0);
  
  // ; Specials
  values (rem, quot) = modf* (0.0);
  is (rem == 0.0);
  is (quot == 0.0);
  
  values (rem, quot) = modf* (-0.0);
  may-fail (rem == -0.0);
  is (quot == -0.0);
  
  if (may-fail (values (rem, quot) = modf* (double-float-positive-infinity))) {
    may-fail (rem == 0.0);
    may-fail (quot == double-float-positive-infinity);
  }
  
  if (may-fail (values (rem, quot) = modf* (double-float-negative-infinity))) {
    may-fail (rem == 0.0);
    may-fail (quot == double-float-negative-infinity);
  }
  
  // ; TODO: add NaN test.
  }#)

(test test-math-scalbn
  #{
  is.float-equal (scalbn (2.0, 2), 8.0);
  is.float-equal (scalbn (-2.0, -2), -.5);
  
  // ; Specials
  is (scalbn (0.0, most-positive-fixnum) == 0.0);
  is (scalbn (-0.0, most-negative-fixnum) == -0.0);
  may-fail (scalbn (double-float-positive-infinity, most-negative-fixnum)
                  == double-float-positive-infinity);
  is (scalbn (double-float-negative-infinity, -1) == double-float-negative-infinity);
  is (scalbn (double-float-negative-infinity, 0) == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-ilogb
  #{
  is (integerp (ilogb (1.0)));
  is (ilogb (0.0) == FP_ILOGB0);
  
  // ; Specials
  is (integerp (ilogb (0.0)));
  may-fail (integerp (ilogb (double-float-negative-infinity)));
  // ; TODO: add NaN test.
  }#)

(test test-math-logb
  #{
  is (floatp (logb (1.0)));
  
  // ; Specials
  is (floatp (logb (0.0)));
  may-fail (floatp (logb (double-float-negative-infinity)));
  // ; TODO: add NaN test.
  }#)

(test test-math-copysign
  #{
  is.float-equal (copysign (1.0, 2.0), 1.0);
  is.float-equal (copysign (1.9999, -1.284), -1.9999);
  is.float-equal (copysign (1.2, 0.0), 1.2);
  is.float-equal (copysign (1.2, -0.0), -1.2);
  
  is (copysign (double-float-positive-infinity, double-float-negative-infinity)
               == double-float-negative-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-isnan
  #{
  is (! isnan (double-float-negative-infinity));
  is (!isnan (0.0));
  // ; TODO: add NaN test.
  }#)

(test test-isinf
  #{
  is (isinf (double-float-positive-infinity));
  is (isinf (double-float-negative-infinity));
  is (!isinf (10.0));
  is (!isinf (0.0));
  // ; TODO: add NaN test.
  }#)

(test test-isfinite
  #{
  is (!isfinite (double-float-positive-infinity));
  is (!isfinite (double-float-negative-infinity));
  is (isfinite (10.0));
  is (isfinite (0.0));
  is (isfinite (least-negative-double-float));
  // ; TODO: add NaN test.
  }#)

(test test-isnormal
  #{
  is (!isnormal (double-float-positive-infinity));
  is (!isnormal (double-float-negative-infinity));
  is (isnormal (10.0));
  is (!isnormal (0.0));
  is (!isnormal (least-negative-double-float));
  // ; TODO: add NaN test.
  }#)

(test test-fpclassify
  #{
  is (fpclassify (double-float-positive-infinity) == FP_INFINITE);
  is (fpclassify (double-float-negative-infinity) == FP_INFINITE);
  is (fpclassify (10.0) == FP_NORMAL);
  is (fpclassify (0.0) == FP_ZERO);
  is (fpclassify (least-negative-double-float) == FP_SUBNORMAL);
  // ; TODO: add NaN test.
  }#)

(test test-signbit
  #{
  is (!signbit (1.0));
  is (signbit (-1.0));
  is (!signbit (double-float-positive-infinity));
  is (signbit (double-float-negative-infinity));
  is (!signbit (0.0));
  is (signbit (-0.0));
  // ; TODO: add NaN test.
  }#)

(test test-isgreater
  #{
  is (|isgreater|(1.0, -1.0));
  is (!|isgreater|(1.0, 1.0));
  is (!|isgreater|(1.0, 9.0));
  is (!|isgreater|(double-float-nan, 1.0));
  is (!|isgreater|(1.0, double-float-nan));
  is (!|isgreater|(double-float-nan, double-float-nan));
  }#)

(test test-isgreaterequal
  #{
  is (|isgreaterequal|(1.0, -1.0));
  is (|isgreaterequal|(1.0, 1.0));
  is (!|isgreaterequal|(1.0, 9.0));
  is (!|isgreaterequal|(double-float-nan, 1.0));
  is (!|isgreaterequal|(1.0, double-float-nan));
  is (!|isgreaterequal|(double-float-nan, double-float-nan));
  }#)

(test test-isless
  #{
  is (!|isless|(1.0, -1.0));
  is (!|isless|(1.0, 1.0));
  is (|isless|(1.0, 9.0));
  is (!|isless|(double-float-nan, 1.0));
  is (!|isless|(1.0, double-float-nan));
  is (!|isless|(double-float-nan, double-float-nan));
  }#)

(test test-islessequal
  #{
  is (!|islessequal|(1.0, -1.0));
  is (|islessequal|(1.0, 1.0));
  is (|islessequal|(1.0, 9.0));
  is (!|islessequal|(double-float-nan, 1.0));
  is (!|islessequal|(1.0, double-float-nan));
  is (!|islessequal|(double-float-nan, double-float-nan));
  }#)

(test test-islessgreater
  #{
  is (|islessgreater|(1.0, -1.0));
  is (!|islessgreater|(1.0, 1.0));
  is (|islessgreater|(1.0, 9.0));
  is (!|islessgreater|(double-float-nan, 1.0));
  is (!|islessgreater|(1.0, double-float-nan));
  is (!|islessgreater|(double-float-nan, double-float-nan));
  }#)

(test test-isunordered
  #{
  is (!|isunordered|(1.0, -1.0));
  is (!|isunordered|(1.0, 1.0));
  is (!|isunordered|(1.0, 9.0));
  is (|isunordered|(double-float-nan, 1.0));
  is (|isunordered|(1.0, double-float-nan));
  is (|isunordered|(double-float-nan, double-float-nan));
  }#)
