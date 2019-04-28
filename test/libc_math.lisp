(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(defmacro is.float-equal (x y)
  `(is (float-equal ,x ,y)))

(defmacro is.float-nan-p (x)
  `(is (float-nan-p ,x)))

(defmacro may-fail (form)
  `(if-let ((result ,form))
     result
     (warn "~A was evaluated to false" ',form)))

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
