(in-package :with-c-syntax.libc-implementation)

(defvar |errno| nil
  "A symbol denoting 'errno' of C, or NIL.
If a function of `with-c-syntax.libc' caught error, it will set the
error to this.")

;;; .. and symbols denotiong errnos are exported. See the package definition.

(defun find-errno-symbol (name)
  ;; XXX:
  ;; I think using the definition of osicat-posix package may be
  ;; useful, but depending on it at ASDF-level is not wanted.  So I
  ;; compromised to this logic..
  (if (find-package :osicat-posix)
      (find-symbol (string name) :osicat-posix)
      (intern name :keyword)))

(defmacro define-errno-symbol (name)
  `(defparameter ,name
     (find-errno-symbol (string ',name))))

(define-errno-symbol EDOM)
(define-errno-symbol ERANGE)
(define-errno-symbol EILSEQ)
