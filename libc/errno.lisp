(in-package :with-c-syntax.libc-implementation)

(defvar |errno| nil
  "A symbol denoting 'errno' of C, or NIL.
If a function of `with-c-syntax.libc' caught error, it will set the
error to this.
(If this has a symbol, that is usable as a condition type of 'osicat-posix'.)")

;;; .. and symbols denotiong errnos are exported. See the package definition.
