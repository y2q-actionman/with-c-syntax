(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(test test-empty
  (is (null #{}#))
  (is (null #{ }#)))

(test test-nil-reading
  (let ((*standard-output* (make-broadcast-stream))) ; dispose output.
    (is (equal #{ format (t, "Hello World!"); }#
	       nil)))
  ;; (2019-2-24) Added for testing NIL reader.
  (is (equal #{ format (nil, "Hello World!"); }#
	     "Hello World!")))

(test test-simple-previous-syntax
  (is (= #{`4 \; }#
         4))
  (is (= #{`(+ 1 2) ;
	 }#
         3)))

(test test-nested-use
  (is (= (let ((x 0))
	   #{
	   int y = x + 1;
	   return `(let ((z (1+ y)))
    		     #{
    		     int ret = z + 10;
    		     return ret;
    		     }#);
	   }#)
	 12)))

(define-symbol-macro unknown-symbol
    '(error "This symbol should not be referenced."))

(test test-reader-aggressive
  ;; '0x'
  (is.equal.wcs #xdEADbEEF
    #1{
    return 0xDeadBeef;
    }#)
  (is.equal.wcs (scale-float (float #x1ff 0d0) (+ -8 1))
    #1{
    return 0x1.ff0p+1;
    }#)
  (is.equal.wcs 124                     ; no octals.
    #1{
    return 00124;
    }#)
  ;; '|'
  (is.equal.wcs 100
    #1{
    return 100 ||unknown-symbol ||`(error "Never comes here.") \;
    }#)
  (is.equal.wcs #x16
    ;; The weird suffix below is for avoiding confusion of syntax highlighting of slime.
    #1{
    return #x12 | #x6 \; // ; |
    }#)
  (is.equal.wcs '|escaped symbol|
    #1{
    return NIL ? `(error "Never comes here.") : '|escaped symbol| \;
    }#)
  ;; { and }
  (is.equal.wcs 99
    #1{{return 99 \;}}#)
  ;; [ and ]
  (is.equal.wcs 2
    #1{
    int hoge-array[]={0,1,2}\;
    return hoge-array[2]\;
    }#)
  ;; `
  (is.equal.wcs 7
    #1{
    return `(+ 4 3);
    }#)
  ;; '.'
  (is.equal.wcs 3
    #1{
    struct{int x;}hoge ={3}\;
    return hoge . x;
    }#)
  (is.equal.wcs (cons 1 2)
    #1{
    return `'(1 . 2);
    }#)
  ;; semicolon
  (is.equal.wcs 3
    #1{{1;2;return 3;}}#
    )
  ;; comments
  (is.equal.wcs 6
    #1{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
      }#)
  (is.equal.wcs 3
    #1{
    int a/b/c = 1 \;
    int /!abc!/ = 2 \;
    return a/b/c + /!abc!/ \;
    }#)
  (is.equal.wcs 1
    #1{
    return 1 //+999999
    \;}#)
  ;; single quote is kept.
  (is.equal.wcs 'a
    #1{
    return 'a \;
    }#)
  (is.equal.wcs '\u00A0
    #1{
    return '\u00A0 \;
    }#)
  ;; double quote
  (is.equal.wcs "abc"
    #1{
    return "abc";
    }#)
  (is.equal.wcs ""
    #1{
    return "";
    }#)
  (is.equal.wcs (coerce '(#\Backspace #\Page #\Newline
			#\Tab #\\ #\' #\" #\?)
		      'string)
    #1{
    return "\b\f\n\t\\\'\"\?";
    }#)
  ;; depends ASCII
  (is.equal.wcs (coerce (list +bel-character+ #\return
			      +vertical-tab-character+)
		      'string)
    #1{
    return "\a\r\v";
    }#)
  (is.equal.wcs (string (code-char #o77))
    #1{
    return "\77";
    }#)
  (is.equal.wcs (coerce '(#.(code-char #o123) #\4)
			'string)
    #1{
    return "\1234";
    }#)
  (is.equal.wcs (string (code-char #x99))
    #1{
    return "\x99";
    }#)
  (is.equal.wcs "$@`"
    #1{
    return "\u0024\u0040\u0060";
    }#)
  ;; parens
  (is.equal.wcs "a"
    #1{
    return string(#\a);
    }#))

(test test-reader-aggressive-default-level
  #.(setf *with-c-syntax-reader-level* 1)
  (is.equal.wcs 99
    #{{return 99 \;}}#)
  (is.equal.wcs 3
    #{{1;2;return 3;}}#
    )
  #.(setf *with-c-syntax-reader-level* with-c-syntax.core::+with-c-syntax-default-reader-level+))

(test test-reader-bad-string-literal
  (signals.wcs.reader (end-of-file) "#1{ return \"; }#")
  (signals.wcs.reader (end-of-file) "#1{ return \"\\\"; }#")
  (signals.wcs.reader () "#1{ return \"
\"; }#"))
