(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(defmacro is.float-equal (x y)
  `(is (float-equal ,x ,y)))

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
  is.float-equal (fabs (0.0), 0.0);
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
  is.float-equal (exp(double-float-negative-infinity), 0);
  is (exp(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-exp2
  #{
  is.float-equal (exp2(1.0), 2);
  is.float-equal (exp2(0.0), 1);
  is.float-equal (exp2(double-float-negative-infinity), 0);
  is (exp2(double-float-positive-infinity) == double-float-positive-infinity);
  // ; TODO: add NaN test.
  }#)

(test test-math-expm1
  #{
  is (`(< 1.71828
         #{ return expm1(1); }#
         1.71829));
  is.float-equal (expm1(0.0), 0);
  is.float-equal (expm1(double-float-negative-infinity), -1.0);
  may-fail ( is (expm1(double-float-positive-infinity) == double-float-positive-infinity));
  // ; TODO: add NaN test.
  }#)







(test test-math-hypot
  #{
  is.float-equal (hypot (1, 1), 1.41421356);
  is.float-equal (hypot (3, 4), 5);
  }#)
