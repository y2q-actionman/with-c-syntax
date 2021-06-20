(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(test test-error-directive
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #error Test
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #error 100 200 0xff.0p1
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #error 
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    # error
    }#)
  )

(test test-null-directive
  (is.equal.wcs #1="# and newline immediately after."
    #2{
    #
    return `#1# \;
    }#)
  (is.equal.wcs #2="#, spaces and newline."
    #2{
    #  
    return `#2# \;
    }#)
  (is.equal.wcs #3="#, line comment, and newline."
    #2{
    # // comment
    return `#3# \;
    }#)
  (is.equal.wcs #4="#, block comment, and newline."
    #2{
    # /* comment */   
    return `#4# \;
    }#)
  (is.equal.wcs #5="#, long block comment, and newline."
    #2{
    # /* comment
    */   
    return `#5# \;
    }#)
  (is.equal.wcs #6="#, long block comment, line comment, and newline."
    #2{
    # /* comment
    */// extra comment
    `#6#
    }#))

(test test-collect-preprocessor-macro-arguments
  (flet ((cpma (x)
	   (with-c-syntax.core::collect-preprocessor-macro-arguments x)))
    (is (equal (cpma '(|(| 1 2 3 |)|))
	       '((1 2 3))))
    (is (equal (cpma '(|(| 1 |,| 2 |,| 3 |)|))
	       '((1) (2) (3))))
    (is (equal (cpma '(|(| int a |,| int b |,| |(| a b c |)| |)|))
	       '((int a) (int b) (|(| a b c |)|))))
    (is (equal (cpma '(|(| |)|))
	       '()))))

(test test-pp-object-like-macro
  (is.equal.wcs 3
    #2{
    #define HOGE (x + y)
    int x = 1;
    int y = 2;
    return HOGE \;
    #undef HOGE // ; TODO: Remove this if a kind of local-macros are implemented.
    }#))

(test test-pp-strcat
  (is.equal.wcs "abc"
    return "a" "b" "c" \; ))

(test test-typedef-hack ()
  (is.equal.wcs 1
    {
    typedef int int_t \;
    int_t x = 1 \;
    return x \;
    }))

;;; TODO: add symbol-interning tests
