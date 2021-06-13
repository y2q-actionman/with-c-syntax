(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(defvar *true-for-suppress-warning* t
  "This value is just `cl:t'.
 I use this for suppressing 'Unused tag' warnings by `cl:assert' of Allegro CL 10.1.")

(define-condition condition-for-testing-libc-assert (simple-error)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf with-c-syntax.libc:NDEBUG nil)
  (test test-assert-ndebug-nil
    #{
    signals (error, |assert| (nil));
    is (cl:or (|assert| (*true-for-suppress-warning*), t));
    ;  // `|assert|' parameter should be evaluated.
    signals (condition-for-testing-libc-assert,
             |assert| (error ('condition-for-testing-libc-assert)));
    }#)
  (makunbound 'with-c-syntax.libc:NDEBUG))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf with-c-syntax.libc:NDEBUG t)
  (test test-assert-ndebug-t
    #{
    is (cl:or (|assert| (nil), t));
    is (cl:or (|assert| ("true"), t));
    ;  // `|assert|' parameter should not be evaluated.
    is (cl:or (|assert| (error ('condition-for-testing-libc-assert)), t));
    }#)
  (makunbound 'with-c-syntax.libc:NDEBUG))

(test test-assert-local
  (assert (not (boundp 'with-c-syntax.libc:NDEBUG)))
  ;; In top-level, `NDEBUG' is left unbound (at default).
  #{
  signals (error, |assert| (nil));
  is (cl:or (|assert| (*true-for-suppress-warning*), t));
  signals (condition-for-testing-libc-assert,
           |assert| (error ('condition-for-testing-libc-assert)));
  }#

  #+with-c-syntax-test-use-compiler-let
  (progn
    ;; binding to NIL -- still signals error.
    (trivial-cltl2:compiler-let ((with-c-syntax.libc:NDEBUG NIL))
      #{
      signals (error, |assert| (nil));
      is (cl:or (|assert| (*true-for-suppress-warning*), t));
      signals (condition-for-testing-libc-assert,
               |assert| (error ('condition-for-testing-libc-assert)));
      }#)

    ;; binding to T -- no error.
    (trivial-cltl2:compiler-let ((with-c-syntax.libc:NDEBUG t))
      #{
      is (cl:or (|assert| (nil), t));
      is (cl:or (|assert| ("true"), t));
      is (cl:or (|assert| (error ('condition-for-testing-libc-assert)), t));
      }#))
  #-with-c-syntax-test-use-compiler-let
  (warn "Your Lisp does not have 'compiler-let' facility. Skip tests.."))
