(in-package #:with-c-syntax.core)

;;; Macro interface
(defmacro with-c-compilation-unit ((entry-form return-last?)
				   &body body)
  "* Syntax
~with-c-compilation-unit~ (entry-form return-last?) &body form* => result*

* Arguments and Values
- entry-form  :: a form
- return-last? :: a boolean
- forms       :: a implicit progn
- results     :: the values returned by forms

* Description
Establishes variable bindings for a new compilation.
"
  `(let ((*struct-specs* (copy-hash-table *struct-specs*))
         (*typedef-names* (copy-hash-table *typedef-names*))
         (*dynamic-binding-requested* nil)
         (*function-pointer-ids* nil)
         (*toplevel-entry-form* ,entry-form)
	 (*return-last-statement* ,return-last?))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-c-syntax (body try-add-{} entry-form return-last?)
    (handler-case
	(with-c-compilation-unit (entry-form return-last?)
	  (parse-with-lexer (list-lexer body) *expression-parser*))
      (yacc-parse-error (condition)
	(if (and try-add-{}
		 (not (starts-with '{ body)))
	    (expand-c-syntax `({ ,@body }) nil entry-form return-last?)	; retry
	    (error 'with-c-syntax-parse-error
		   :yacc-error condition))))))

(defmacro with-c-syntax ((&rest options
			  &key (case-sensitive
                                (ecase (readtable-case *readtable*)
                                  ((:upcase :downcase) nil)
                                  ((:preserve :invert) t)))
			  (return :auto)
			  (try-add-{} t))
			 &environment *wcs-expanding-environment*
			 &body body)
  "This macro is a entry point of the with-c-syntax system. BODY will be
interpreted as C syntax, executed, and return values.


CASE-SENSITIVE specifies case-sensitivity in interpreting symbols.
If nil, C syntactic keyworks and Libc functions are treated case-insentisively.

If RETURN is `:auto', returns the last form's value if BODY is a
compound statement. (If BODY is a compilation unit, this returns NIL
now, but this behavior may be changed.)

If RETURN is any other value, its valus is inserted after the
compilation result translation units. (This feature is intended to
access 'static' variables.)

?f TRY-ADD-{} is t and an error occurred at parsing, `with-c-syntax'
adds '{' and '}' into the head and tail of FORM respectively, and
tries to parse again."
  (cond
    ((null body)
     nil)
    ((and (length= 1 (the list body))	; with-c-syntax is nested.
	  (starts-with 'with-c-syntax (first body)))
     (destructuring-bind (op_ options2 &body body2)
	 (first body)
       (declare (ignore op_))
       `(with-c-syntax (,@options ,@options2) ,@body2)))
    (t
     (expand-c-syntax (preprocessor body case-sensitive)
		      try-add-{}
		      (if (eq return :auto) nil return)
		      (eq return :auto)))))
