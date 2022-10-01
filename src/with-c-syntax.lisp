(in-package #:with-c-syntax.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-c-syntax (body try-add-{})
    (handler-case
	(with-c-compilation-unit
            (:return-last-statement (eql (first body) '{))
	  (parse-with-lexer (list-lexer body) *expression-parser*))
      (yacc-parse-error (condition)
	(if (and try-add-{}
		 (not (starts-with '{ body)))
	    (expand-c-syntax `({ ,@body }) nil) ; retry
	    (error 'with-c-syntax-parse-error
		   :yacc-error condition))))))

(defmacro with-c-syntax ((&rest options
			  &key (preprocess t)
                            (reader-level *with-c-syntax-reader-level*)
                            (readtable-case (or *with-c-syntax-reader-case*
                                                (readtable-case *readtable*)))
                            (input-file-pathname nil)
			    (try-add-{} t))
			 &environment *wcs-expanding-environment*
			 &body body)
  "This macro is a entry point of the with-c-syntax system. BODY will be
interpreted as C syntax, executed, and return values.

This macro returns the last form's value if BODY is a compound
statement, or returns NIL if BODY is a compilation unit.

PREPROCESS specifies how to do preprocess. If nil, this macro compiles
BODY without preprocessing. If t, preprocesses and compiles BODY. If
`:preprocess-only', this macro preprocesses BODY and returns the
result as-is.

READER-LEVEL specifies the reader level (see `*with-c-syntax-reader-level*').
This is used when '#include' or '_Pragma()' was used.

READTABLE-CASE specifies the readtable case of symbols in BODY when it
was read.  This affects how to intern C keywords by the preprocessor.

INPUT-FILE-PATHNAME is passed to the preprocessor and used when
'__FILE__' macro was used.

If TRY-ADD-{} is t and an error occurred at parsing, `with-c-syntax'
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
     (ecase preprocess
       (:preprocess-only
        (preprocessor body reader-level readtable-case input-file-pathname))
       (t
        `(with-c-syntax (:preprocess nil :try-add-{} ,try-add-{})
           ,@(preprocessor body reader-level readtable-case input-file-pathname)))
       ((nil)
        (expand-c-syntax body
		         try-add-{}))))))
