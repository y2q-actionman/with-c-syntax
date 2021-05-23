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
  (is.equal.wcs (code-char #xa0)
    #1{
    return '\u00A0'\;
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
  (is.equal.wcs (coerce (list (code-char #x07) (code-char #x0d)
			     (code-char #x0b))
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
    return string('a');
    }#)
  ;; check default-level
  #.(setf *with-c-syntax-reader-level* 1)
  (is.equal.wcs 99
    #{{return 99 \;}}#)
  (is.equal.wcs 3
    #{{1;2;return 3;}}#
    ))

(test test-reader-aggressive-bad-char
  (signals.wcs.reader (end-of-file) "#1{ return \"; }#")
  (signals.wcs.reader (end-of-file) "#1{ return \"\\\"; }#")
  (signals.wcs.reader () "#1{ return \"
\"; }#")
  ;; 
  (signals.wcs.reader (end-of-file) "#1{ return '")
  (signals.wcs.reader () "#1{ return ''; }#")
  (signals.wcs.reader () "#1{ return '
'; }#")
  (signals.wcs.reader () "#1{ return '; }#")
  (signals.wcs.reader () "#1{ return '\\'; }#")
  (signals.wcs.reader () "#1{ return '\\Y'; }#")
  (signals.wcs.reader () "#1{ return '\\x; }#")
  (signals.wcs.reader () "#1{ return '\\u; }#")
  (signals.wcs.reader () "#1{ return '\\u12; }#")
  (signals.wcs.reader () "#1{ return '\\U1234; }#")
  (signals.wcs.reader () "#1{ return '\\u0060; }#")
  (signals.wcs.reader () "#1{ return '\\U0000D800; }#")
  (signals.wcs.reader () "#1{ return '\\uDFFF; }#")
  (signals.wcs.reader () "#1{ return '\\99'; }#")
  ;; Below will make a two-character integer constant (\022 and 3).
  ;; See "6.4.4.4 Character constants" 14 Example 3.
  (signals.wcs.reader () "#1{ return '\\0223; }#"))

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

(test test-reader-overkill-2
  ;; Some numeric operator combinations.
  (is.equal.wcs t
    #2{
    return 1+2+3-4+5+6+78+9==100
    && 1*2*3*4+5+6+7*8+9==100
    && 1*2*3-4*5+6*7+8*9==100
    && -1+2-3+4+5+6+78+9==100
    && 98-76+54+3+21==100
    && 98+7-6*5+4*3*2+1==100;
    }#)
  (is.equal.wcs t
    #2{
    int x = 96+1428/357;
    return x == 3+69258/714;
    }#)
  ;; 
  )

(test test-reader-numeric-literal
  ;; decimal integers
  (is.equal.wcs 0
    #2{
    return 0;
    }#)
  (is.equal.wcs 10
    #2{
    return 10;
    }#)
  (is.equal.wcs 10
    #2{
    return 10;
    }#)
  ;; octal integers
  (is.equal.wcs #o0
    #2{
    return 00;
    }#)
  (is.equal.wcs #o10
    #2{
    return 010;
    }#)
  (is.equal.wcs #o11
    #2{
    return 011;
    }#)
  ;; hexadecimal integers
  (is.equal.wcs #x0
    #2{
    return 0x00;
    }#)
  (is.equal.wcs #x10
    #2{
    return 0x10;
    }#)
  (is.equal.wcs #xdeadbeef
    #2{
    return 0xDeadBeef;
    }#)
  ;; suffixes
  (is.equal.wcs 0
    #2{
    return 0u;
    }#)
  (is.equal.wcs 1
    #2{
    unsigned int u = 1U;
    return u;
    }#)
  (is.equal.wcs -2
    #2{
    return -2l;
    }#)
  (is.equal.wcs 3
    #2{
    long l = 3L;
    return l;
    }#)
  (is.equal.wcs 4
    #2{
    return 4ul;
    }#)
  (is.equal.wcs 5
    #2{
    int LU = 5LU;
    return LU;
    }#)
  (is.equal.wcs 6
    #2{
    return 6ll;
    }#)
  (is.equal.wcs -7
    #2{
    long long int LL = 7LL;
    return -LL;
    }#)
  (is.equal.wcs 8
    #2{
    unsigned long long int ull = 8ull;
    return ull;
    }#)
  (is.equal.wcs 9
    #2{
    long long unsigned int llu = 9llu;
    return llu;
    }#)
  (is.equal.wcs 10
    #2{
    return 10Ull;
    }#)
  (is.equal.wcs 11
    #2{
    return 11llU;
    }#)
  (is.equal.wcs 12
    #2{
    return 12uLL;
    }#)
  (is.equal.wcs 13
    #2{
    return 13LLu;
    }#)
  (is.equal.wcs 14
    #2{
    return 14ULL;
    }#)
  (is.equal.wcs 15
    #2{
    return 15LLU;
    }#)
  
  (is.equal.wcs #xABC
    #2{
    return 0xabcULL;
    }#)
  (is.equal.wcs #o42
    #2{
    return 042uLL;
    }#)

  ;; minus operator ('-' not a part of integer literal.)
  (is.equal.wcs -0
    #2{
    return -0;
    }#)
  (is.equal.wcs #x-100
    #2{
    return -0x100l;
    }#)
  (is.equal.wcs #x-100
    #2{
    long long int ll = -0x100ll;
    return ll;
    }#)
  )

(test test-reader-numeric-bad-suffix
  (signals.wcs.reader () "#2{ return 0uu; }#")    
  (signals.wcs.reader () "#2{ return 0lll; }#")
  ;; (signals.wcs.reader () "#2{ return 1xx; }#")
  (signals.wcs.reader () "#2{ return 0lL; }#")
  (signals.wcs.reader () "#2{ return 0Ll; }#")
  (signals.wcs.reader () "#2{ return 0Lul; }#")
  (signals.wcs.reader () "#2{ return 0uuL; }#")
  ;; 
  )

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
