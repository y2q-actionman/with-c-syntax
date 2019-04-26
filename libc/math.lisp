(in-package #:with-c-syntax.libc-implementation)

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
  (signum x))

;;; C++20

;;; I found it in https://cpprefjp.github.io/reference/cmath/lerp.html
;;; and Alexardia has it!
(defun |lerp| (a b v)
  (alexandria:lerp v a b))
