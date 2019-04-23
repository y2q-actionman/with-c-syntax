(in-package #:with-c-syntax.libc)

(defvar NDEBUG)

;; I think there is no way to specify any docstrings when `defvar'
;; left the varible unbound. (But see `defvar-unbound' in serapeum.)
(setf (documentation 'NDEBUG 'variable)
      "This variable is seen by |assert|. Please refer |assert|'s docstring.")


(defmacro |assert| (&rest args)
  "This emulates 'assert' of the C language.
If `NDEBUG' is bound and its value is true, `|assert|' will be
expanded to a no-op.  If not (`NDEBUG' is unbound or bound to NIL),
`|assert|' will be expanded to `cl:assert'.

This macro sees `NDEBUG' value only when macroexpanding-time, not in runtime.
(If you want to change `NDEBUG' locally, you should use `compiler-let'.)"
  (if (and (boundp 'NDEBUG)
	   (symbol-value 'NDEBUG))
      '(values)
      `(assert ,@args)))
