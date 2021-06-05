(in-package #:with-c-syntax.libc-implementation)

;;; Utilities.

(define-modify-macro logiorf (&rest args)
  logior)

(define-modify-macro logandc2f (i)
  logandc2)
