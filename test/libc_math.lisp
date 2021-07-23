(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(defmacro check-errno (form expected-errno
                       &key (alternate-errno nil aeno-supplied-p))
  `(let ((|errno| nil))
     (prog1 ,form
       ,@(if (not aeno-supplied-p)
             `((is (eql |errno| ,expected-errno)))
             `((is (or (eql |errno| ,expected-errno)
                       (eql |errno| ,alternate-errno)))
               (unless (eql |errno| ,expected-errno)
                 (warn "Form ~A; errno resulted to ~A, not ~A"
                       ',form |errno| ,expected-errno)))))))

(defmacro is.float-equal (x y)
  `(is (float-equal ,x ,y)))

(test test-fpclassify
  #{
  is (|fpclassify| (double-float-positive-infinity) == FP_INFINITE);
  is (|fpclassify| (double-float-negative-infinity) == FP_INFINITE);
  is (|fpclassify| (10.0) == FP_NORMAL);
  is (|fpclassify| (0.0) == FP_ZERO);
  is (|fpclassify| (least-negative-double-float) == FP_SUBNORMAL);
  is (|fpclassify| (NAN) == FP_NAN);
  }#)

(test test-isfinite
  #{
  is (! |isfinite| (double-float-positive-infinity));
  is (! |isfinite| (double-float-negative-infinity));
  is (|isfinite| (10.0));
  is (|isfinite| (0.0));
  is (|isfinite| (least-negative-double-float));
  is (! |isfinite| (NAN));
  }#)

(test test-isinf
  #{
  is (|isinf| (double-float-positive-infinity));
  is (|isinf| (double-float-negative-infinity));
  is (! |isinf| (10.0));
  is (! |isinf| (0.0));
  is (! |isinf| (least-negative-double-float));
  is (! |isinf| (NAN));
  }#)

(test test-isnan
  #{
  is (! |isnan| (double-float-positive-infinity));
  is (! |isnan| (double-float-negative-infinity));
  is (! |isnan| (10.0));
  is (! |isnan| (0.0));
  is (! |isnan| (least-negative-double-float));
  is (|isnan| (NAN));
  }#)

(test test-isnormal
  #{
  is (! |isnormal| (double-float-positive-infinity));
  is (! |isnormal| (double-float-negative-infinity));
  is (|isnormal| (10.0));
  is (! |isnormal| (0.0));
  is (! |isnormal| (least-negative-double-float));
  is (! |isnormal| (NAN));
  }#)

(test test-signbit
  #{
  is (! |signbit| (1.0));
  is (|signbit| (-1.0));
  is (! |signbit| (double-float-positive-infinity));
  is (|signbit| (double-float-negative-infinity));
  is (! |signbit| (0.0));
  if (eq (0.0d0, -0.0d0)) {
    `(warn "Your Lisp does not distinguish -0.0d0 from 0.0d0.");
  } else {
    is (|signbit| (-0.0));
  }
  check-errno (|signbit| (NAN), nil);
  }#)

(test test-math-acos
  #{
  is.float-equal (|acos| (1.0), 0.0);
  is.float-equal (|acos| (|sqrt| (3) / 2), PI / 6);
  is.float-equal (|acos| (0.5), PI / 3);
  is.float-equal (|acos| (0), PI / 2);
  // ; Specials
  check-errno (is (|acos| (1.0001) == NAN), EDOM);
  check-errno (is (|acos| (-1.00000001010) == NAN), EDOM);
  check-errno (is (|acos| (double-float-positive-infinity) == NAN), EDOM);
  check-errno (is (|acos| (double-float-negative-infinity) == NAN), EDOM);
  check-errno (is (float-nan-p (|acos| (NAN))), nil);
  }#)

(test test-math-asin
  #{
  is.float-equal (|asin| (0.0), 0.0);
  is.float-equal (|asin| (0.5), PI / 6);
  is.float-equal (|asin| (- |sqrt| (3) / 2), -PI / 3);
  is.float-equal (|asin| (1.0), PI / 2);
  // ; Specials
  is (|asin| (-0.0) == -0.0);
  check-errno (is (|asin| (1.0001) == NAN), EDOM);
  check-errno (is (|asin| (-1.00000001010) == NAN), EDOM);
  check-errno (is (|asin| (double-float-positive-infinity) == NAN), EDOM);
  check-errno (is (|asin| (double-float-negative-infinity) == NAN), EDOM);
  check-errno (is (float-nan-p (|asin| (NAN))), nil);
  }#)

(test test-math-atan
  #{
  is.float-equal (|atan| (0.0), 0.0);
  is.float-equal (|atan| (1 / |sqrt| (3)), PI / 6);
  is.float-equal (|atan| (- |sqrt| (3)), -PI / 3);
  // ; Specials
  check-errno (is (|atan| (-0.0) == -0.0), nil);
  check-errno (is.float-equal (|atan| (double-float-positive-infinity), PI / 2), nil);
  check-errno (is.float-equal (|atan| (double-float-negative-infinity), -PI / 2), nil);
  check-errno (is (float-nan-p (|atan| (NAN))), nil);
  }#)

(test test-math-atan2
  #{
  is.float-equal (|atan2| (0.0, 1), 0.0);
  is.float-equal (|atan2| (1, |sqrt| (3)), PI / 6);
  is.float-equal (|atan2| (-1, 1 / |sqrt| (3)), -PI / 3);
  // ; Specials
  if (eq (0.0d0, -0.0d0)) {
    `(warn "Your Lisp does not distinguish -0.0d0 from 0.0d0.");
  } else {
    is (|atan2| (0.0, -0.0) == PI);
    is (|atan2| (-0.0, -0.0) == -PI);
  }
  is (|atan2| (0.0, 0.0) == 0.0);
  is (|atan2| (-0.0, 0.0) == -0.0);
  check-errno (is (|atan2| (double-float-positive-infinity, 10) == PI / 2), nil);
  check-errno (is (|atan2| (double-float-negative-infinity, -100) == -PI / 2), nil);
  check-errno (is (|atan2| (double-float-positive-infinity, double-float-negative-infinity) == PI * 3 / 4), nil);
  check-errno (is (|atan2| (double-float-negative-infinity, double-float-positive-infinity) == -PI / 4), nil);
  check-errno (is (|atan2| (-9999, -0.0) == - PI / 2), nil);
  check-errno (is (|atan2| (9999, -0.0) == PI / 2), nil);
  check-errno (is (|atan2| (9999, double-float-negative-infinity) == PI), nil);
  check-errno (is (|atan2| (-9999, double-float-negative-infinity) == - PI), nil);
  check-errno (is (|atan2| (9999, double-float-positive-infinity) == 0.0), nil);
  check-errno (is (|atan2| (-9999, double-float-positive-infinity) == -0.0), nil);
  check-errno (is (float-nan-p (|atan2| (NAN, 1))), nil);
  check-errno (is (float-nan-p (|atan2| (1, NAN))), nil);
  }#)

(test test-math-cos
  #{
  is.float-equal (|cos| (0.0), 1.0);
  is.float-equal (|cos| (PI / 6),  |sqrt| (3) / 2);
  is.float-equal (|cos| (-PI / 3), 0.5);
  is.float-equal (|cos| (PI / 2), 0);
  // ; Specials
  is (|cos| (-0.0) == 1.0);
  check-errno (is (float-nan-p (|cos| (double-float-positive-infinity))), EDOM);
  check-errno (is (float-nan-p (|cos| (double-float-negative-infinity))), EDOM);
  check-errno (is (float-nan-p (|cos| (NAN))), nil);
  }#)

(test test-math-sin
  #{
  is.float-equal (|sin| (0.0), 0.0);
  is.float-equal (|sin| (PI / 6), 0.5);
  is.float-equal (|sin| (-PI / 3), - |sqrt| (3) / 2);
  is.float-equal (|sin| (PI / 2), 1.0);
  // ; Specials
  is (|sin| (-0.0) == -0.0);
  check-errno (is (float-nan-p (|sin| (double-float-positive-infinity))), EDOM);
  check-errno (is (float-nan-p (|sin| (double-float-negative-infinity))), EDOM);
  check-errno (is (float-nan-p (|sin| (NAN))), nil);
  }#)

(test test-math-tan
  #{
  double d, expected;
  
  is.float-equal (|tan| (0.0), 0.0);
  
  d = |tan| (PI / 6);
  expected = 1 / |sqrt| (3);
  if (d != expected) {
    `(warn "Failed: tan(PI/6) actual ~A expected ~A" d expected);
  }

  d = |tan| (-PI / 3);
  expected = - |sqrt| (3);
  if (d != expected) {
    `(warn "Failed: tan(-PI/3) actual ~A expected ~A" d expected);
  }
  
  // ; Specials

  // ; Allegro CL 10.0 fails this.
  |errno| = nil;
  d = |tan| (PI / 2);
  if (float-infinity-p (d)) {
    is (|errno| == ERANGE);
  } else {
    `(warn "Your Lisp returns a finite value ~A for tan(PI/2)." d);
  }
  
  is (|tan| (-0.0) == -0.0);
  check-errno (is (float-nan-p (|tan| (double-float-positive-infinity))), EDOM);
  check-errno (is (float-nan-p (|tan| (double-float-negative-infinity))), EDOM);
  check-errno (is (float-nan-p (|tan| (NAN))), nil);
  }#)

(test test-math-acosh
  #{
  is.float-equal (|acosh| (|cosh| (10.0)), 10.0);
  is.float-equal (|acosh| (|cosh| (1.1)), 1.1);
  // ; Specials
  check-errno (is (float-nan-p (|acosh| (double-float-negative-infinity))), EDOM);
  check-errno (is (float-nan-p (|acosh| (most-negative-double-float))), EDOM);
  check-errno (is (float-nan-p (|acosh| (-3.1))), EDOM);
  check-errno (is (float-nan-p (|acosh| (0.9))), EDOM);
  check-errno (is (|acosh| (1.0) == 0.0), nil);
  check-errno (is (|acosh| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (float-nan-p (|acosh| (NAN))), nil);
  }#)

(test test-math-asinh
  #{
  is.float-equal (|asinh| (|sinh| (10.0)), 10.0);
  is.float-equal (|asinh| (|sinh| (-1.0)), -1.0);
  // ; Specials
  check-errno (is (|asinh| (0.0) == 0.0), nil);
  check-errno (is (|asinh| (-0.0) == -0.0), nil);
  check-errno (is (|asinh| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|asinh| (double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|asinh| (NAN))), nil);
  }#)

(test test-math-atanh
  #{
  is.float-equal (|atanh| (|tanh| (.9)), .9);
  is.float-equal (|atanh| (|tanh| (-.1)), -.1);
  // ; Specials
  check-errno (is (|atanh| (0.0) == 0.0), nil);
  check-errno (is (|atanh| (-0.0) == -0.0), nil);
  check-errno (is (float-infinity-p (|atanh| (1.0))), ERANGE);
  check-errno (is (float-infinity-p (|atanh| (-1.0))), ERANGE);
  check-errno (is (float-nan-p (|atanh| (double-float-positive-infinity))), EDOM);
  check-errno (is (float-nan-p (|atanh| (1.1))), EDOM);
  check-errno (is (float-nan-p (|atanh| (-1.1))), EDOM);
  check-errno (is (float-nan-p (|atanh| (double-float-negative-infinity))), EDOM);
  is (float-nan-p (|atanh| (NAN)));
  }#)

(test test-math-cosh
  #{
  is.float-equal (|cosh| (2.3), (|exp| (2.3) + |exp| (-2.3)) / 2);
  is.float-equal (|cosh| (-9), (|exp| (-9) + |exp| (- - 9)) / 2);
  check-errno (is (|cosh| (1000.0) == double-float-positive-infinity), ERANGE);
  // ; Specials
  check-errno (is (|cosh| (0.0) == 1.0), nil);
  check-errno (is (|cosh| (-0.0) == 1.0), nil);
  check-errno (is (|cosh| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|cosh| (double-float-negative-infinity) == double-float-positive-infinity), nil);
  check-errno (is (float-nan-p (|cosh| (NAN))), nil);
  }#)

(test test-math-sinh
  #{
  is.float-equal (|sinh| (2.3), (|exp| (2.3) - |exp| (-2.3)) / 2);
  is.float-equal (|sinh| (-9), (|exp| (-9) - |exp| (- - 9)) / 2);
  check-errno (is (|sinh| (1000.0) == double-float-positive-infinity), ERANGE);
  // ; Specials
  check-errno (is (|sinh| (0.0) == 0.0), nil);
  check-errno (is (|sinh| (-0.0) == -0.0), nil);
  check-errno (is (|sinh| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|sinh| (double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|sinh| (NAN))), nil);
  }#)

(test test-math-tanh
  #{
  is.float-equal (|tanh| (2.3), (|exp| (2.3) - |exp| (-2.3)) / (|exp| (2.3) + |exp| (-2.3)));
  is.float-equal (|tanh| (-9), (|exp| (-9) - |exp| (- - 9)) / (|exp| (-9) + |exp| (- - 9)));
  // ; Specials
  check-errno (is (|tanh| (0.0) == 0.0), nil);
  check-errno (is (|tanh| (-0.0) == -0.0), nil);
  check-errno (is (|tanh| (double-float-positive-infinity) == 1.0), nil);
  check-errno (is (|tanh| (double-float-negative-infinity) == -1.0), nil);
  check-errno (is (float-nan-p (|tanh| (NAN))), nil);
  }#)

(test test-math-exp
  #{
  is (`(< 2.71828
         #{ return |exp|(1.0d0); }#
         2.71829));
  // ; TODO: I require implicit numeric type conversions to write it like exp(0);
  is.float-equal (|exp|(0.0d0), 1.0d0);
  check-errno (is(|exp|(most-positive-double-float) == HUGE_VAL), ERANGE);
//  check-errno (is(|exp|(most-negative-double-float) == 0.0), ERANGE);
  check-errno (is(|exp|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is(|exp|(double-float-negative-infinity) == 0.0), nil);
  check-errno (is (float-nan-p (|exp|(NAN))), nil);
  }#)

(test test-math-exp2
  #{
  is.float-equal (|exp2|(2.0), 4.0);
  is (|exp2|(0.0) == 1.0);
  check-errno (is (|exp2|(most-positive-double-float) == HUGE_VAL), ERANGE);
  check-errno (is (|exp2|(most-negative-double-float) == 0.0), ERANGE);
  check-errno (is (|exp2|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is (|exp2|(double-float-negative-infinity) == 0.0), nil);
  check-errno (is (float-nan-p (|exp2|(NAN))), nil);
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
//   check-errno (is (|expm1|(double-float-positive-infinity) == HUGE_VAL), nil);
  check-errno (is (|expm1|(double-float-negative-infinity) == -1.0), nil, :alternate-errno, ERANGE);
  check-errno (is (float-nan-p (|expm1|(NAN))), nil);
  }#)

(test test-math-frexp*-and-ldexp
  #{
  double d;
  int e;

  values (d, e) = frexp* (3.14);
  is (0.5 <= d && d < 1);
  is.float-equal (|ldexp| (d, e), 3.14);
  
  values (d, e) = frexp* (-10.001);
  is (-0.5 >= d && d > -1);
  is.float-equal (|ldexp| (d, e), -10.001);
  
  // ; Specials (frexp*)
  values (d, e) = frexp* (0.0);
  is (d == 0.0);
  is (e == 0);
  values (d, e) = frexp* (-0.0);
  is (d == -0.0);
  is (e == 0);
  check-errno (values (d, e) = frexp* (double-float-positive-infinity), nil);
  is (d == double-float-positive-infinity);
  check-errno (values (d, e) = frexp* (double-float-negative-infinity), nil);
  is (d == double-float-negative-infinity);
  check-errno (values (d, e) = frexp* (NAN), nil);
  is (float-nan-p (d));

  // ; Specials (|ldexp|)
  check-errno (is (|ldexp| (0.0, most-positive-fixnum) == 0.0), nil);
  check-errno (is (|ldexp| (-0.0, most-negative-fixnum) == -0.0), nil);
  check-errno (is (|ldexp| (least-positive-double-float, most-positive-fixnum) == 0.0), ERANGE);
  check-errno (is (|ldexp| (most-negative-double-float, most-positive-fixnum) == double-float-negative-infinity), ERANGE);
  check-errno (is (|ldexp| (double-float-positive-infinity, most-negative-fixnum) == double-float-positive-infinity), nil);
  check-errno (is (|ldexp| (double-float-negative-infinity, -1) == double-float-negative-infinity), nil);
  check-errno (is (|ldexp| (double-float-negative-infinity, 0) == double-float-negative-infinity), nil);
  is (float-nan-p (|ldexp| (NAN, 0)));
  }#)

(test test-math-ilogb
  #{
  is (integerp (|ilogb| (1.0)));
  is (|ilogb| (4.0) == 2);
  
  // ; Specials
  check-errno (is (|ilogb| (0.0) == FP_ILOGB0), EDOM, :alternate-errno, nil);
  check-errno (is (|ilogb| (double-float-positive-infinity) == INT_MAX), EDOM);
  check-errno (is (|ilogb| (double-float-negative-infinity) == INT_MAX), EDOM);
  check-errno (is (|ilogb| (NAN) == FP_ILOGBNAN), EDOM);
  }#)

(test test-math-log
  #{
  is.float-equal (|log| (|exp| (2.0)), 2);
  check-errno (is (|log|(1.0) == 0.0), nil);
//  check-errno (is (|log|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log|(-1.0) == NAN), EDOM);
  check-errno (is (|log|(-HUGE_VAL) == NAN), EDOM);
  check-errno (is (|log|(NAN) == NAN), nil);
  }#)

(test test-math-log10
  #{
  is.float-equal (|log10| (100.0), 2);
  check-errno (is (|log10|(1.0) == 0.0), nil);
//   check-errno (is (|log10|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log10|(-1.0) == NAN), EDOM);
  check-errno (is (|log10|(-HUGE_VAL) == NAN), EDOM);
  check-errno (is (|log10|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log10|(NAN) == NAN), nil);
  }#)

(test test-math-log1p
  #{
  is.float-equal (|log1p| (11.0), |log| (12.0));
  check-errno (is (|log1p|(0.0) == 0.0), nil);
  is (|log1p|(-0.5) < 0);
  // check-errno (is (|log1p|(-1.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log1p|(-2.0)) == NAN, EDOM);
  check-errno (is (|log1p|(-HUGE_VAL) == NAN), EDOM);
  check-errno (is (|log1p|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log1p|(NAN) == NAN), nil);
  }#)

(test test-math-log2
  #{
  is.float-equal (|log2| (8.0), 3);
  check-errno (is (|log2|(1.0) == 0.0), nil);
  // check-errno (is (|log2|(0.0) == -HUGE_VAL), ERANGE);
  check-errno (is (|log2|(-1.0) == NAN), EDOM);
  check-errno (is (|log2|(-HUGE_VAL) == NAN), EDOM);
  check-errno (is (|log2|(HUGE_VAL) == double-float-positive-infinity), nil);
  check-errno (is (|log2|(NAN) == NAN), nil);
  }#)

(test test-math-logb
  #{
  is (floatp (|logb| (1.0)));
  is.float-equal (|logb| (4.0), 2.0);
  
  // ; Specials
  // ; TODO: FIXME: `float' is overwritten because it is a C keyword.
  // ; I think some workaround is needed here.
  check-errno (is (|logb| (0.0) == `(float FP_ILOGB0 0d0)), EDOM, :alternate-errno, nil);
  check-errno (is (|logb| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|logb| (double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (float-nan-p (|logb| (NAN)), nil);
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
  
//   values (rem, quot) = modf* (-0.0);
//  is (rem == -0.0);
//  is (quot == -0.0);
  
//  check-errno (values (rem, quot) = modf* (double-float-positive-infinity), nil);
//  is (rem == 0.0);
//  is (quot == double-float-positive-infinity);
  
//  check-errno (values (rem, quot) = modf* (double-float-negative-infinity), nil);
//  is (rem == 0.0);
//  is (quot == double-float-negative-infinity);
  
//  check-errno (values (rem, quot) = modf* (NAN), nil);
//  is (float-nan-p (rem));
//  is (float-nan-p (quot));
  }#)

(test test-math-scalbn
  #{
  is.float-equal (|scalbn| (2.0, 2), 8.0);
  is.float-equal (|scalbn| (-2.0, -2), -.5);
  
  // ; Specials
  check-errno (is (|scalbn| (0.0, most-positive-fixnum) == 0.0), nil);
  check-errno (is (|scalbn| (-0.0, most-negative-fixnum) == -0.0), nil);
  check-errno (is (|scalbn| (least-positive-double-float, most-positive-fixnum) == 0.0), ERANGE);
  check-errno (is (|scalbn| (most-negative-double-float, most-positive-fixnum) == double-float-negative-infinity), ERANGE);
  check-errno (is (|scalbn| (double-float-positive-infinity, most-negative-fixnum) == double-float-positive-infinity), nil);
  check-errno (is (|scalbn| (double-float-negative-infinity, -1) == double-float-negative-infinity), nil);
  check-errno (is (|scalbn| (double-float-negative-infinity, 0) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|scalbn| (NAN, 1))), nil);
  }#)

(test test-math-cbrt
  #{
  double tmp;
  
  is.float-equal (|cbrt| (1000.0), 10.0);
  
  /*
  is (|cbrt| (0.0) == 0.0);
  is (|cbrt| (-0.0) == -0.0);
  check-errno (is (|cbrt| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|cbrt| (NAN) == NAN), nil);
  
  // ; FIXME: cbrt(-1.0) may return a complex. Under implementation...
  check-errno (tmp = |cbrt| (-1.0), nil);
  is (tmp == -1.0 || complexp (tmp));

  check-errno (tmp = |cbrt| (double-float-negative-infinity), nil);
  is (tmp == double-float-negative-infinity || complexp (tmp));
  */
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
  is (float-nan-p (|fabs|(NAN)));
  }#)

(test test-math-hypot
  #{
  is.float-equal (|hypot| (1, 1), 1.414213562373095145474621858739);
  is.float-equal (|hypot| (3, 4), 5);
  is.float-equal (|hypot| (1.23, -4.56), |hypot| (4.56, -1.23));
  is.float-equal (|hypot| (1.23, 0), |fabs| (1.23));
  is.float-equal (|hypot| (-0, -9928.123456), |fabs| (-9928.123456));

  check-errno (is (|hypot| (most-positive-double-float, most-positive-double-float)
                           == double-float-positive-infinity),
                  ERANGE);
  is (|hypot| (least-positive-double-float, least-positive-double-float) > 0);
  
  is (|hypot| (double-float-negative-infinity, 0) == double-float-positive-infinity);
  is (|hypot| (0, double-float-positive-infinity) == double-float-positive-infinity);
  // is (|hypot| (NAN, double-float-positive-infinity) == double-float-positive-infinity);
  is (|hypot| (-1, NAN) == NAN);
  }#)

(test test-math-pow
  #{
  double d;
  
  is (float-equal (|pow| (2, 3), 8.0));
  is (float-equal (|pow| (-1.1, 2), 1.21));
  is (float-equal (|pow| (-1.1, -2), `(/ 1.21)));

  is (|pow| (0.0, 1) == 0.0);
  is (|pow| (-0.0, 1) == -0.0);
  is (|pow| (0.0, 2) == 0.0);
  is (|pow| (-0.0, 2.5) == 0.0);
  
  // ; Errors and specials
  check-errno (is (float-nan-p (|pow| (-2.1, 0.3))), EDOM);
  
  check-errno (is (|pow| (most-positive-double-float, 810.0) == HUGE_VAL), ERANGE);
  check-errno (is (|pow| (most-negative-double-float, 810.0) == HUGE_VAL), ERANGE);
  check-errno (d = |pow| (most-negative-double-float, 811.0), ERANGE);
  is (d == - HUGE_VAL || d == HUGE_VAL);
  
  check-errno (is (|pow| (least-positive-double-float, 810.0) == 0d0), ERANGE);
  
  check-errno (is (float-nan-p (|pow| (2, NAN))), nil);
  check-errno (is (float-nan-p (|pow| (NAN, -42))), nil, :alternate-errno, EDOM);
  
//  check-errno (is (|pow| (1, double-float-positive-infinity) == 1.0), nil);
//  check-errno (is (|pow| (1, double-float-negative-infinity) == 1.0), nil);
//  check-errno (is (|pow| (1, NAN) == 1.0), nil);

  check-errno (is (|pow| (1, 0) == 1.0), nil);
//  check-errno (is (|pow| (double-float-positive-infinity, 0) == 1.0), nil);
//  check-errno (is (|pow| (double-float-negative-infinity, 0) == 1.0), nil);
//  check-errno (is (|pow| (NAN, 0) == 1.0), nil);
  
  check-errno (is (|pow| (0, 1) == 0d0), nil);
  check-errno (is (|pow| (-0d0, 101) == -0d0), nil);
  
  check-errno (is (|pow| (0d0, 114514) == 0d0), nil);
  check-errno (is (|pow| (-0d0, 114514) == 0d0), nil);
  
  // ; Linux man page says this is +1.0.
  |errno| = nil;
  d = |pow| (-1, double-float-positive-infinity);
  is ((d == 1d0 && |errno| == nil) || (float-nan-p (d) && |errno| == EDOM));
  
  // ; Linux man page says this is +1.0.
  |errno| = nil;
  d = |pow| (-1, double-float-negative-infinity);
  is ((d == 1d0 && |errno| == nil) || float-nan-p (d) && |errno| == EDOM);
  
  check-errno (is (|pow| (0.9999, double-float-negative-infinity) == double-float-positive-infinity), nil, :alternate-errno, ERANGE);
  
  // ; Linux man page says this is +Inf.
  |errno| = nil;
  d = |pow| (-0.9999, double-float-negative-infinity);
  // is ((d == double-float-positive-infinity && |errno| == ERANGE) || float-nan-p (d) && |errno| == EDOM);
  
  check-errno (is (|pow| (-0.0, double-float-negative-infinity) == double-float-positive-infinity),
                  ERANGE, :alternate-errno, nil); // Linux man page says errno == 0.
  
  check-errno (is (|pow| (1.0001, double-float-negative-infinity) == 0d0), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (0.9999, double-float-positive-infinity) == 0d0), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (1.0001, double-float-positive-infinity) == double-float-positive-infinity), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (double-float-negative-infinity, -3) == -0.0), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (double-float-negative-infinity, -2) == 0.0), nil, :alternate-errno, ERANGE);
  check-errno (is (|pow| (double-float-negative-infinity, -2.1) == 0.0), nil, :alternate-errno, ERANGE);

  // ; Linux man page says this is -Inf.
  check-errno (is (|pow| (double-float-negative-infinity, 3) == double-float-negative-infinity), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (double-float-negative-infinity, 2) == double-float-positive-infinity), nil);
  check-errno (is (|pow| (double-float-negative-infinity, 2.1) == double-float-positive-infinity), nil);
  // ; Linux man page says this is +Inf
  |errno| = nil;
  d = |pow| (double-float-negative-infinity, double-float-positive-infinity);
  is ((d == double-float-positive-infinity && |errno| == nil) || (float-nan-p (d) && |errno| == EDOM));
  
  check-errno (is (|pow| (double-float-positive-infinity, -10) == 0.0), nil, :alternate-errno, ERANGE);
  check-errno (is (|pow| (double-float-positive-infinity, double-float-negative-infinity) == 0.0), nil, :alternate-errno, ERANGE);
  
  check-errno (is (|pow| (double-float-positive-infinity, 10) == double-float-positive-infinity), nil);
  check-errno (is (|pow| (double-float-positive-infinity, double-float-positive-infinity) == double-float-positive-infinity), nil);
  
  // check-errno (is (|pow| (0.0, -1) == double-float-positive-infinity), ERANGE);
  // ; Linux man page says this is -Inf.
  // check-errno (is (float-infinity-p (|pow| (-0.0, -3))), ERANGE);
  
  // check-errno (is (|pow| (0.0, -2) == double-float-positive-infinity), ERANGE);
  // check-errno (is (|pow| (-0.0, -2.5) == double-float-positive-infinity), ERANGE);

  is (|pow| (NAN, 2) == NAN);
  is (|pow| (-1, NAN) == NAN);
  is (|pow| (NAN, NAN) == NAN);
  }#)

(test test-math-sqrt
  #{
  is.float-equal (|sqrt| (100.0), 10.0);
  // ; Specials
  is (|sqrt| (0.0) == 0.0);
  is (|sqrt| (-0.0) == -0.0);
  check-errno (is (|sqrt| (double-float-positive-infinity) == double-float-positive-infinity),nil);
  check-errno (is (float-nan-p (|sqrt| (-2.1))), EDOM);
  check-errno (is (float-nan-p (|sqrt| (double-float-negative-infinity))), EDOM);
  check-errno (is (float-nan-p (|sqrt| (NAN))), nil);
  }#)

(test test-math-ceil
  #{
  is.float-equal (|ceil| (3.14), 4.0);
  is.float-equal (|ceil| (-3.14), -3.0);
  // ; Specials
  check-errno (is (|ceil| (0.0) == 0.0), nil);
  check-errno (is (|ceil| (-0.0) == -0.0), nil);
  check-errno (is (|ceil|(double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|ceil|(double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|ceil| (NAN))), nil);
  }#)

(test test-math-floor
  #{
  is.float-equal (|floor| (3.14), 3.0);
  is.float-equal (|floor| (-3.14), -4.0);
  // ; Specials
  check-errno (is (|floor| (0.0) == 0.0), nil);
  check-errno (is (|floor| (-0.0) == -0.0), nil);
  check-errno (is (|floor|(double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|floor|(double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|floor| (NAN))), nil);
  }#)

(test test-math-round
  #{
  is.float-equal (|round| (3.14), 3.0);
  is.float-equal (|round| (3.9), 4.0);
  is.float-equal (|round| (-3.14), -3.0);
  is.float-equal (|round| (-3.9), -4.0);
  // ; Specials
  /*
  check-errno (is (|round| (0.0) == 0.0), nil);
  check-errno (is (|round| (-0.0) == -0.0), nil);
  check-errno (is (|round| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|round| (double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|round| (NAN))), nil);
  */
  }#)

(test test-math-lround
  #{
  is (|lround| (3.14) == 3);
  is (|lround| (3.9) == 4);
  is (|lround| (-3.14) == -3);
  is (|lround| (-3.9) == -4);
  // ; Specials
  /*
  check-errno (is (|lround| (0.0) == 0), nil);
  check-errno (is (|lround| (-0.0) == 0), nil);
  is (|lround| (double-float-positive-infinity) != double-float-positive-infinity);
  is (|lround| (double-float-negative-infinity) != double-float-negative-infinity);
  is (! float-nan-p (|lround| (NAN)));
  */
  }#)

(test test-math-trunc
  #{
  is.float-equal (|trunc| (3.14), 3.0);
  is.float-equal (|trunc| (3.9), 3.0);
  is.float-equal (|trunc| (-3.14), -3.0);
  is.float-equal (|trunc| (-3.9), -3.0);
  // ; Specials
  check-errno (is (|trunc| (0.0) == 0.0), nil);
  check-errno (is (|trunc| (-0.0) == -0.0), nil);
  check-errno (is (|trunc| (double-float-positive-infinity) == double-float-positive-infinity), nil);
  check-errno (is (|trunc| (double-float-negative-infinity) == double-float-negative-infinity), nil);
  check-errno (is (float-nan-p (|trunc| (NAN))), nil);
  }#)

(test test-math-fmod
  #{
  is.float-equal (|fmod|(3.123, 2.0), 1.123);
  is.float-equal (|fmod|(-3.123, 2.0), -1.123);
  is.float-equal (|fmod|(3.123, -2.0), 1.123);
  is.float-equal (|fmod|(-3.123, -2.0), -1.123);
  
  // ; Specials
  is (|fmod|(0.0, 2.0) == 0.0);
  // is (|fmod|(-0.0, 2.0) == -0.0);
  is.float-equal (|fmod|(-0.0, 2.0), -0.0);
  check-errno (is (float-nan-p (|fmod|(99.0, 0.0))), EDOM);
  check-errno (is (float-nan-p (|fmod|(double-float-positive-infinity, 10.0))), EDOM);
  check-errno (is (float-nan-p (|fmod|(NAN, 1.0))), nil);
  check-errno (is (float-nan-p (|fmod|(1.0, NAN))), nil);
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
  check-errno (is (float-nan-p (|remainder|(NAN, 1.0))), nil);
  check-errno (is (float-nan-p (|remainder|(1.0, NAN))), nil);
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
  check-errno (is (float-nan-p (remquo* (NAN, +1.0))), nil);
  check-errno (is (float-nan-p (remquo* (1.0, NAN))), nil);
  }#)

;;; FIXME: I have |NAN| and |nan| in Libc package. So this code misses which to use.
#+ ()
(test test-math-nan
  #{
  is (float-nan-p (|nan|("")));
  }#)

(test test-math-copysign
  #{
  is.float-equal (|copysign| (1.0, 2.0), 1.0);
  is.float-equal (|copysign| (1.9999, -1.284), -1.9999);
  is.float-equal (|copysign| (1.2, 0.0), 1.2);
  if (eq (0.0d0, -0.0d0)) {
    `(warn "Your Lisp does not distinguish -0.0d0 from 0.0d0.");
  } else {
    is.float-equal (|copysign| (1.2, -0.0), -1.2);
  }

  is (|copysign| (double-float-positive-infinity, double-float-negative-infinity)
               == double-float-negative-infinity);
  is (float-nan-p (|copysign| (NAN, -1)));
  check-errno (|copysign| (1, NAN), nil);
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
       (warn "Your Lisp's double-float representation is not expected one of this test.")));

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
  check-errno (is (float-nan-p (|nextafter|(NAN, NAN))), nil);
  check-errno (is (float-nan-p (|nextafter|(NAN, 1.0))), nil);
  check-errno (is (float-nan-p (|nextafter|(1.0, NAN))), nil);
  }#)

(test test-math-nexttoward
  #{
  is (|nexttoward|(1.5d0,  10d0) == `#2{ 0x1.8000000000001p+0; }#
                 );
  is (|nexttoward|(1.5d0, -10d0) == `#2{ 0x1.7ffffffffffffp+0; }#
                 );
  is (|nexttoward|(1.0d0,  10d0) == `#2{ 0x1.0000000000001p+0; }#
                  );

  if (least-positive-double-float != least-positive-long-float) {
    is (|nexttoward|(least-positive-double-float, least-positive-long-float) == 0d0);
  } else {
    `(warn "Your Lisp's double-float may be same with long-float.");
  }
  
  // ; Others are in nextafter() case.
  }#)

(test test-math-fdim
  #{
  is (|fdim|(2.0, 1.0) == 1.0);
  is (|fdim|(1.0, 2.0) == 0.0);
  is (|fdim|(double-float-positive-infinity, 1.0) == double-float-positive-infinity);
  check-errno (is (|fdim|(most-positive-double-float, most-negative-double-float) == HUGE_VAL),
                  ERANGE);
  check-errno (is (float-nan-p (|fdim|(-1.0, NAN))), nil);
  }#)

(test test-math-fmax
  #{
  is (|fmax|(3.125, 2.0) == 3.125);
  is (|fmax|(double-float-negative-infinity, 2.0) == 2.0);
  is (|fmax|(double-float-positive-infinity, 2.0) == double-float-positive-infinity);
  is (|fmax|(NAN, 1.0) == 1.0);
  is (|fmax|(-1.0, NAN) == -1.0);
  is (float-nan-p (|fmax|(NAN, NAN)));
  }#)

(test test-math-fmin
  #{
  is (|fmin|(3.125, 2.0) == 2.0);
  is (|fmin|(double-float-negative-infinity, 2.0) == double-float-negative-infinity);
  is (|fmin|(double-float-positive-infinity, 2.0) == 2.0);
  is (|fmin|(NAN, 1.0) == 1.0);
  is (|fmin|(-1.0, NAN) == -1.0);
  is (float-nan-p (|fmin|(NAN, NAN)));
  }#)

(test test-isgreater
  #{
  is (|isgreater|(1.0, -1.0));
  is (! |isgreater|(1.0, 1.0));
  is (! |isgreater|(1.0, 9.0));
  is (! |isgreater|(NAN, 1.0));
  is (! |isgreater|(1.0, NAN));
  is (! |isgreater|(NAN, NAN));
  }#)

(test test-isgreaterequal
  #{
  is (|isgreaterequal|(1.0, -1.0));
  is (|isgreaterequal|(1.0, 1.0));
  is (! |isgreaterequal|(1.0, 9.0));
  is (! |isgreaterequal|(NAN, 1.0));
  is (! |isgreaterequal|(1.0, NAN));
  is (! |isgreaterequal|(NAN, NAN));
  }#)

(test test-isless
  #{
  is (! |isless|(1.0, -1.0));
  is (! |isless|(1.0, 1.0));
  is (|isless|(1.0, 9.0));
  is (! |isless|(NAN, 1.0));
  is (! |isless|(1.0, NAN));
  is (! |isless|(NAN, NAN));
  }#)

(test test-islessequal
  #{
  is (! |islessequal|(1.0, -1.0));
  is (|islessequal|(1.0, 1.0));
  is (|islessequal|(1.0, 9.0));
  is (! |islessequal|(NAN, 1.0));
  is (! |islessequal|(1.0, NAN));
  is (! |islessequal|(NAN, NAN));
  }#)

(test test-islessgreater
  #{
  is (|islessgreater|(1.0, -1.0));
  is (! |islessgreater|(1.0, 1.0));
  is (|islessgreater|(1.0, 9.0));
  is (! |islessgreater|(NAN, 1.0));
  is (! |islessgreater|(1.0, NAN));
  is (! |islessgreater|(NAN, NAN));
  }#)

(test test-isunordered
  #{
  is (! |isunordered|(1.0, -1.0));
  is (! |isunordered|(1.0, 1.0));
  is (! |isunordered|(1.0, 9.0));
  is (|isunordered|(NAN, 1.0));
  is (|isunordered|(1.0, NAN));
  is (|isunordered|(NAN, NAN));
  }#)
