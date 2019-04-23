(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter with-c-syntax.libc:NDEBUG nil)
  (test test-assert-ndebug-nil
    #{
    signals (error, assert (nil));
    is (progn (assert ("true"), t));
    }#)
  (makunbound 'with-c-syntax.libc:NDEBUG))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter with-c-syntax.libc:NDEBUG t)
  (test test-assert-ndebug-t
    #{
    is (progn (assert (nil), t));
    is (progn (assert ("true"), t));
    }#)
  (makunbound 'with-c-syntax.libc:NDEBUG))

(test test-assert-local
  (assert (not (boundp 'with-c-syntax.libc:NDEBUG)))
  ;; In top-level, `NDEBUG' is left unbound (at default).
  #{
  signals (error, assert (nil));
  is (progn (assert ("true"), t));
  }#

  #+with-c-syntax-test-use-compiler-let
  (progn
    ;; binding to NIL -- still signals error.
    (trivial-cltl2:compiler-let ((with-c-syntax.libc:NDEBUG NIL))
      #{
      signals (error, assert (nil));
      is (progn (assert ("true"), t));
      }#)

    ;; binding to T -- no error.
    (trivial-cltl2:compiler-let ((with-c-syntax.libc:NDEBUG t))
      #{
      is (progn (assert (nil), t));
      is (progn (assert ("true"), t));
      }#))
  #-with-c-syntax-test-use-compiler-let
  (warn "Your Lisp does not have 'compiler-let' facility. Skip tests.."))
