;;; Currently, floating-point error emulation only.

(in-package #:with-c-syntax.libc-implementation)

;;; Internals

(defstruct wcs-fexcept
  (flags 0 :type fixnum)
  (operation-name nil :type symbol))

(defun test-wcs-fexcept-flags (excepts wcs-fexcept)
  (logtest excepts
           (wcs-fexcept-flags wcs-fexcept)))

(defun assign-wcs-fexcept (dst src)
  (setf (wcs-fexcept-flags dst) (wcs-fexcept-flags src)
        (wcs-fexcept-operation-name dst) (wcs-fexcept-operation-name src))
  dst)

;;; Types

(add-typedef '|fexcept_t| 'wcs-fexcept)

;;; Constants

(defconstant FE_DIVBYZERO #b1)
;; (defconstant FE_INEXACT #b10) ; TODO
(defconstant FE_INVALID #b100)
(defconstant FE_OVERFLOW #b1000)
(defconstant FE_UNDERFLOW #b10000)

(defconstant FE_ALL_EXCEPT
  (logior FE_DIVBYZERO FE_INVALID FE_OVERFLOW FE_UNDERFLOW))

;;; fe.*except family

(defvar *fexcept-register*
  (make-wcs-fexcept)
  "Internal storage of error flag for our floating-point error emulation.")

(defun |feclearexcept| (excepts)
  (cond ((zerop excepts)
         t)
        ((test-wcs-fexcept-flags excepts *fexcept-register*)
         (logandc2f (wcs-fexcept-flags *fexcept-register*)
                    excepts))
        (t nil)))

(defun |fegetexceptflag| (flagp excepts)
  (cond ((test-wcs-fexcept-flags excepts *fexcept-register*)
         (assign-wcs-fexcept flagp *fexcept-register*))
        (t nil)))

(defun |feraiseexcept| (excepts)
  (logiorf (wcs-fexcept-flags *fexcept-register*) excepts))

(defun |fesetexceptflag| (flagp excepts)
  (cond ((zerop excepts)
         t)
        ((test-wcs-fexcept-flags excepts flagp)
         ;; FIXME: this assignment is suspicious.
         (assign-wcs-fexcept *fexcept-register* flagp))
        (t nil)))

(defun |fetestexcept| (excepts)
  (logand (wcs-fexcept-flags *fexcept-register*)
          excepts))


(defun |fegetround| ()
  FLT_ROUNDS)
