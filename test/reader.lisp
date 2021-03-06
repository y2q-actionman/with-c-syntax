(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

;;; These reader variables should be evaluated in read-time.
#.(setf *with-c-syntax-reader-level* nil)
#.(setf *with-c-syntax-reader-case* nil)

(test test-nil-reading
  (let ((*standard-output* (make-broadcast-stream))) ; dispose output.
    (is (equal #{ format (t, "Hello World!"); }#
	       nil)))
  ;; (2019-2-24) Added for testing NIL reader.
  (is (equal #{ format (nil, "Hello World!"); }#
	     "Hello World!")))

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

#.(setf *with-c-syntax-reader-level* 0)
(test test-reader-conservative
  ;; comma
  (is.equal.wcs 2
    #0{
    int hoge-array [ ] = { 0,1,2 } \;
    return hoge-array [ 2 ] \;
    }#)
  ;; ':'
  (is.equal.wcs 2
    #0{
    return 1 ? 2 : 3 \;
    }#)
  ;; check default-level
  #.(setf *with-c-syntax-reader-level* 0)
  (is.equal.wcs 2
    #{
    return 1 ? 2 : 3 \;
    }#))

(test test-reader-aggressive
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
    #1{{1;2;return 3;}}#)
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
  ;; single quote
  (is.equal.wcs #\a
    #1{
    return 'a'\;
    }#)
  ;; double quote
  (is.equal.wcs "abc"
    #1{
    return "abc";
    }#)
  (is.equal.wcs (coerce '(#\Backspace #\Page #\Newline
			#\Tab #\\ #\' #\" #\?)
		      'string)
    #1{
    return "\b\f\n\t\\\'\"\?";
    }#)
  ;; depends ASCII
  (is.equal.wcs (coerce (list (code-char #x07) (code-char #x0d)
			     (code-char #x0b))
		      'string)
    #1{
    return "\a\r\v";
    }#)
  (is.equal.wcs (string (code-char 99))
    #1{
    return "\99";
    }#)
  (is.equal.wcs (string (code-char #x99))
    #1{
    return "\x99";
    }#)
  ;; parens
  (is.equal.wcs "a"
    #1{
    return string('a');
    }#)
  ;; check default-level
  #.(setf *with-c-syntax-reader-level* 1)
  (is.equal.wcs 99
    #{{return 99 \;}}#)
  (is.equal.wcs 3
    #{{1;2;return 3;}}#
    ))

(test test-reader-overkill
 (let ((x 2) (y 3)) 
  ;; comments
  (is.equal.wcs 6
    #2{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  ;; ? :
  (muffle-unused-code-warning
    (is.equal.wcs 2
      #2{
      return 1?x:y;
      }#))
  ;; ~
  (is.equal.wcs (lognot 2)
    #2{
    return ~x;
    }#)
  ;; =
  (is.equal.wcs 2
    #2{
    int hoge=x; return hoge;
    }#)
  ;; ==
  (is.equal.wcs nil
    #2{
    return x==y;
    }#)
  ;; *
  (is.equal.wcs 6
    #2{
    return x*y;
    }#)
  ;; *=
  (is.equal.wcs 6
    #2{
    int hoge=x; hoge*=y; return hoge;
    }#)
  ;; /
  (is.equal.wcs (/ 2 3)
    #2{
    return x/y;
    }#)
  ;; /=
  (is.equal.wcs (/ 2 3)
    #2{
    int hoge=x; hoge/=y; return hoge;
    }#)
  ;; %
  (is.equal.wcs 2
    #2{
    return x%y;
    }#)
  ;; %=
  (is.equal.wcs 2
    #2{
    int hoge=x; hoge%=y; return hoge;
    }#)
  ;; ^
  (is.equal.wcs (logxor 2 3)
    #2{
    return x^y;
    }#)
  ;; ^=
  (is.equal.wcs (logxor 2 3)
    #2{
    int hoge=x; hoge^=y; return hoge;
    }#)
  ;; !
  (is.equal.wcs nil
    #2{
    return !x;
    }#)
  ;; !=
  (is.equal.wcs t
    #2{
    return x!=y;
    }#)
  ;; &
  (is.equal.wcs (logand 2 3)
    #2{
    return x&y;
    }#)
  ;; &=
  (is.equal.wcs (logand 2 3)
    #2{
    int hoge=x; hoge&=y; return hoge;
    }#)
  ;; &&
  (is.equal.wcs 3
    #2{
    return x&&y;
    }#)
  ;; |
  (is.equal.wcs (logior 2 3)
    #2{
    return x|y;
    }#)
  ;; |=
  (is.equal.wcs (logior 2 3)
    #2{
    int hoge=x; hoge|=y; return hoge;
    }#)
  ;; ||
  (muffle-unused-code-warning
    (is.equal.wcs 2
      #2{
      return x||y;
      }#))
  ;; +
  (is.equal.wcs 1
    #2{
    return +1;
    }#)
  (is.equal.wcs 5
    #2{
    return x+y;
    }#)
  ;; +=
  (is.equal.wcs 5
    #2{
    int hoge=x; hoge+=y; return hoge;
    }#)
  ;; -
  (is.equal.wcs -1
    #2{
    return -1;
    }#)
  (is.equal.wcs -1
    #2{
    return x-y;
    }#)
  ;; -=
  (is.equal.wcs -1
    #2{
    int hoge=x; hoge-=y; return hoge;
    }#)
  ;; <
  (is.equal.wcs t
    #2{
    return x<y;
    }#)
  ;; <=
  (is.equal.wcs t
    #2{
    return x<=y;
    }#)
  ;; <<
  (is.equal.wcs (ash 2 3)
    #2{
    return x<<y;
    }#)
  ;; <<=
  (is.equal.wcs (ash 2 3)
    #2{
    int hoge=x; hoge<<=y; return hoge;
    }#)
  ;; >
  (is.equal.wcs nil
    #2{
    return x>y;
    }#)
  ;; >=
  (is.equal.wcs nil
    #2{
    return x>=y;
    }#)
  ;; >>
  (is.equal.wcs (ash 2 -3)
    #2{
    return x>>y;
    }#)
  ;; >>=
  (is.equal.wcs (ash 2 -3)
    #2{
    int hoge=x; hoge>>=y; return hoge;
    }#)

  ;; .
  (is.equal.wcs 3
    #2{
    struct{int x;}hoge={3};
    return hoge.x;
    }#)
  ;; ->
  (is.equal.wcs 3
    #2{
    struct{int x;}hoge={3};
    return (&hoge)->x;
    }#)

  ;; check default-level
  #.(setf *with-c-syntax-reader-level* 2)
  (is.equal.wcs 6
    #{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)))


#.(setf *with-c-syntax-reader-level* nil)

#0{
int test-reader-toplevel-conservative \( \) {
  return t \;
}
}#

#1{
int test-reader-toplevel-aggressive(){
  int hoge-array[]={0,1,2};
  return hoge-array[2]== 2;
}
}#

#2{
int test\-reader\-toplevel\-overkill(){
  assert (1+2*3-4 == `(+ 1 (* 2 3) (- 4)));
  return t;
}
}#

(test test-toplevel-reader
  (is (test-reader-toplevel-conservative))
  (is (test-reader-toplevel-aggressive))
  (is (test-reader-toplevel-overkill)))

#.(setf *with-c-syntax-reader-level* nil)
#.(setf *with-c-syntax-reader-case* :preserve)
(test test-reader-case-sensitivity
  (is.equal.wcs nil
    #{
    int x \, X \;
    x = 1 \;
    X = 2 \;
    return x == X \;
    }#))

;;; Agh, I need file-local variable..
#.(setf *with-c-syntax-reader-level* nil)
#.(setf *with-c-syntax-reader-case* nil)
