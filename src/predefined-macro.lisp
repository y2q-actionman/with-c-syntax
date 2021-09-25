(in-package #:with-c-syntax.core)

(define-constant +pp-date-month-name+
    #("(bad month)"
      "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  :test 'equalp
  :documentation "A vector of month names following asctime() manner. Used by __DATE__")

(defun with-c-syntax.predefined-macro:__DATE__ (&optional state)
  (declare (ignore state))
  (multiple-value-bind (_second _minute _hour date month year)
      (get-decoded-time)
    (declare (ignore _second _minute _hour))
    (format nil "~A ~2,' D ~4,'0D"
            (aref +pp-date-month-name+ month) date year)))

(defun with-c-syntax.predefined-macro:__FILE__ (state)
  (if-let ((file-pathname (pp-state-file-pathname state)))
    (namestring file-pathname)))

(defun with-c-syntax.predefined-macro:__LINE__ (state)
  (pp-state-line-number state))

(defconstant with-c-syntax.predefined-macro:__STDC__ 1)

(defconstant with-c-syntax.predefined-macro:__STDC_HOSTED__ 0)

(defconstant with-c-syntax.predefined-macro:__STDC_MB_MIGHT_NEQ_WC__ 1)

(defconstant with-c-syntax.predefined-macro:__STDC_VERSION__ 199901)

(defun with-c-syntax.predefined-macro:__TIME__ (&optional state)
  (declare (ignore state))
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
