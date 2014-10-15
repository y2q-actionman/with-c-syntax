(in-package #:with-c-syntax.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +operators+
      '(|,|
	= *= /= %= += -= <<= >>= &= ^= \|=
	? |:|
	\|\|
	&&
	\|
	^
	&
	== !=
	< > <= >=
	>> <<
	+ -
	* / %
	\( \)
	++ -- |sizeof|
	& * + - ~ !
	[ ] \. ->)
    :test 'equal
    :documentation
    "* Value Type
a list :: consists of symbols.

* Description
This constant holds a list of symbols denoting C operators.
")

  (define-constant +keywords+
      '(\;
	|auto| |register| |static| |extern| |typedef|
	|void| |char| |short| |int| |long|
        |float| |double| |signed| |unsigned|
	|const| |volatile|
	|struct| |union|
	|enum|
	|...|
	|case| |default|
	{ }
	|if| |else| |switch|
	|while| |do| |for|
	|goto| |continue| |break| |return|
        |__lisp_type|
        |__offsetof|)
    :test 'equal
    :documentation
    "* Value Type
a list :: consists of symbols.

* Description
This constant holds a list of symbols denoting C keywords and keywords
of extension of with-c-syntax.
")
  )

(define-constant +numeric-types-alist+
    '(;; Extension: uses T if no types are specified
      (()				.	t)
      ;; Intergers
      ((|int|)                          .	fixnum)
      ((|int| |short|)                  .	(signed-byte 16))
      ((|int| |long|)                   .	(signed-byte 32))
      ((|int| |long| |long|)            .	(signed-byte 64))
      ((|int| |signed|)                 .	fixnum)
      ((|int| |short| |signed|)         .	(signed-byte 16))
      ((|int| |long| |signed|)          .	(signed-byte 32))
      ((|int| |long| |long| |signed|)   .	(signed-byte 64))
      ((|int| |unsigned| )              .	(integer 0 #.(max most-positive-fixnum 65535)))
      ((|int| |short| |unsigned|)       .	(unsigned-byte 16))
      ((|int| |long| |unsigned|)        .	(unsigned-byte 32))
      ((|int| |long| |long| |unsigned|) .	(unsigned-byte 64))
      ;; Intergers, but 'int' is emitted
      ((|short|)			.	(signed-byte 16))
      ((|long|)				.	(signed-byte 32))
      ((|long| |long|)			.	(signed-byte 64))
      ((|signed|)			.	fixnum)
      ((|short| |signed|)		.	(signed-byte 16))
      ((|long| |signed|)		.	(signed-byte 32))
      ((|long| |long| |signed|)		.	(signed-byte 64))
      ((|unsigned| )			.	(integer 0 #.(max most-positive-fixnum 65535)))
      ((|short| |unsigned|)		.	(unsigned-byte 16))
      ((|long| |unsigned|)		.	(unsigned-byte 32))
      ((|long| |long| |unsigned|)	.	(unsigned-byte 64))
      ;; Char
      ((|char|)                         .	(signed-byte 8))
      ((|char| |signed|)                .	(signed-byte 8))
      ((|char| |unsigned|)              .	(unsigned-byte 8))
      ;; Float
      ((|float|)                        .	single-float)
      ((|float| |short|)                .	short-float)
      ((|double|)                       .	double-float)
      ((|double| |long|)                .	long-float))
  :test 'equal
  :documentation
  "* Value Type
a list :: consists of alists -- (list-of-symbols . <lisp-type>)

* Description
This constant holds relationships between notations of C type and
Common Lisp types.
For each entry of alist, the car is sorted alphabetically.
")
