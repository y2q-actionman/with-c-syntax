(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(defun test-nil-reading ()
  (let ((*standard-output* (make-broadcast-stream))) ; dispose output.
    (assert (equal #{ format (t, "Hello World!"); }#
		   nil)))
  ;; (2019-2-24) Added for testing NIL reader.
  (assert (equal #{ format (nil, "Hello World!"); }#
		 "Hello World!"))
  t)

#.(setf *with-c-syntax-reader-level* :conservative)
(defun test-reader-conservative ()
  ;; comma
  (eval-equal 2 ()
    #0{
    int hoge-array [ ] = { 0,1,2 } \;
    return hoge-array [ 2 ] \;
    }#)
  ;; ':'
  (eval-equal 2 ()
    #0{
    return 1 ? 2 : 3 \;
    }#)
  ;; check default-level
  (eval-equal 2 ()
    #{
    return 1 ? 2 : 3 \;
    }#)
  t)

#.(setf *with-c-syntax-reader-level* :aggressive)
(defun test-reader-aggressive ()
  ;; { and }
  (eval-equal 99 ()
    #1{{return 99 \;}}#)
  ;; [ and ]
  (eval-equal 2 ()
    #1{
    int hoge-array[]={0,1,2}\;
    return hoge-array[2]\;
    }#)
  ;; check default-level
  (eval-equal 99 ()
    #{{return 99 \;}}#)
  t)

#.(setf *with-c-syntax-reader-level* :overkill)
(defun test-reader-overkill ()
  ;; `
  (eval-equal 7 ()
    #2{
    return `(+ 4 3);
    }#)
  ;; '.'
  (eval-equal 3 ()
    #2{
    struct{int x;}hoge ={3}\;
    return hoge . x;
    }#)
  (eval-equal (cons 1 2) ()
    #2{
    return `'(1 . 2);
    }#)
  ;; semicolon
  (eval-equal 3 ()
    #2{{1;2;return 3;}}#)
  ;; comments
  (eval-equal 6 ()
    #2{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
      }#)
  (eval-equal 3 ()
    #2{
    int a/b/c = 1 \;
    int /!abc!/ = 2 \;
    return a/b/c + /!abc!/ \;
    }#)
  (eval-equal 1 ()
    #2{
    return 1 //+999999
    \;}#)
  ;; single quote
  (eval-equal #\a ()
    #2{
    return 'a'\;
    }#)
  ;; double quote
  (eval-equal "abc" ()
    #2{
    return "abc";
    }#)
  (eval-equal (coerce '(#\Backspace #\Page #\Newline
			#\Tab #\\ #\' #\" #\?)
		      'string)
      ()
    #2{
    return "\b\f\n\t\\\'\"\?";
    }#)
  ;; depends ASCII
  (eval-equal (coerce (list (code-char #x07) (code-char #x0d)
			     (code-char #x0b))
		      'string)
      ()
    #2{
    return "\a\r\v";
    }#)
  (eval-equal (string (code-char 99)) ()
    #2{
    return "\99";
    }#)
  (eval-equal (string (code-char #x99)) ()
    #2{
    return "\x99";
    }#)
  ;; parens
  (eval-equal "a" ()
    #2{
    return string('a');
    }#)
  ;; check default-level
  (eval-equal 3 ()
    #{{1;2;return 3;}}#
    )
  t)

#.(setf *with-c-syntax-reader-level* :insane)
(defun test-reader-insane (&aux (x 2) (y 3))
  ;; comments
  (eval-equal 6 ()
    #3{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  ;; ? :
  (muffle-unused-code-warning
    (eval-equal 2 ()
      #3{
      return 1?x:y;
      }#))
  ;; ~
  (eval-equal (lognot 2) ()
    #3{
    return ~x;
    }#)
  ;; =
  (eval-equal 2 ()
    #3{
    int hoge=x; return hoge;
    }#)
  ;; ==
  (eval-equal nil ()
    #3{
    return x==y;
    }#)
  ;; *
  (eval-equal 6 ()
    #3{
    return x*y;
    }#)
  ;; *=
  (eval-equal 6 ()
    #3{
    int hoge=x; hoge*=y; return hoge;
    }#)
  ;; /
  (eval-equal (/ 2 3) ()
    #3{
    return x/y;
    }#)
  ;; /=
  (eval-equal (/ 2 3) ()
    #3{
    int hoge=x; hoge/=y; return hoge;
    }#)
  ;; %
  (eval-equal 2 ()
    #3{
    return x%y;
    }#)
  ;; %=
  (eval-equal 2 ()
    #3{
    int hoge=x; hoge%=y; return hoge;
    }#)
  ;; ^
  (eval-equal (logxor 2 3) ()
    #3{
    return x^y;
    }#)
  ;; ^=
  (eval-equal (logxor 2 3) ()
    #3{
    int hoge=x; hoge^=y; return hoge;
    }#)
  ;; !
  (eval-equal nil ()
    #3{
    return !x;
    }#)
  ;; !=
  (eval-equal t ()
    #3{
    return x!=y;
    }#)
  ;; &
  (eval-equal (logand 2 3) ()
    #3{
    return x&y;
    }#)
  ;; &=
  (eval-equal (logand 2 3) ()
    #3{
    int hoge=x; hoge&=y; return hoge;
    }#)
  ;; &&
  (eval-equal 3 ()
    #3{
    return x&&y;
    }#)
  ;; |
  (eval-equal (logior 2 3) ()
    #3{
    return x|y;
    }#)
  ;; |=
  (eval-equal (logior 2 3) ()
    #3{
    int hoge=x; hoge|=y; return hoge;
    }#)
  ;; ||
  (muffle-unused-code-warning
    (eval-equal 2 ()
      #3{
      return x||y;
      }#))
  ;; +
  (eval-equal 1 ()
    #3{
    return +1;
    }#)
  (eval-equal 5 ()
    #3{
    return x+y;
    }#)
  ;; +=
  (eval-equal 5 ()
    #3{
    int hoge=x; hoge+=y; return hoge;
    }#)
  ;; -
  (eval-equal -1 ()
    #3{
    return -1;
    }#)
  (eval-equal -1 ()
    #3{
    return x-y;
    }#)
  ;; -=
  (eval-equal -1 ()
    #3{
    int hoge=x; hoge-=y; return hoge;
    }#)
  ;; <
  (eval-equal t ()
    #3{
    return x<y;
    }#)
  ;; <=
  (eval-equal t ()
    #3{
    return x<=y;
    }#)
  ;; <<
  (eval-equal (ash 2 3) ()
    #3{
    return x<<y;
    }#)
  ;; <<=
  (eval-equal (ash 2 3) ()
    #3{
    int hoge=x; hoge<<=y; return hoge;
    }#)
  ;; >
  (eval-equal nil ()
    #3{
    return x>y;
    }#)
  ;; >=
  (eval-equal nil ()
    #3{
    return x>=y;
    }#)
  ;; >>
  (eval-equal (ash 2 -3) ()
    #3{
    return x>>y;
    }#)
  ;; >>=
  (eval-equal (ash 2 -3) ()
    #3{
    int hoge=x; hoge>>=y; return hoge;
    }#)

  ;; .
  (eval-equal 3 ()
    #3{
    struct{int x;}hoge={3};
    return hoge.x;
    }#)
  ;; ->
  (eval-equal 3 ()
    #3{
    struct{int x;}hoge={3};
    return (&hoge)->x;
    }#)

  ;; check default-level
  (eval-equal 6 ()
    #{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  t)


#.(setf *with-c-syntax-reader-level* nil)

#0{
int test-reader-toplevel-conservative \( \) {
  return t \;
}
}#

#1{
int test-reader-toplevel-aggressive \( \){
  return t \;
}
}#

#2{
int test-reader-toplevel-overkill(){
  int hoge-array[]={0,1,2};
  return hoge-array[2]== 2;
}
}#

#3{
int test\-reader\-toplevel\-insane(){
  assert (1+2*3-4 == `(+ 1 (* 2 3) (- 4)));
  return t;
}
}#

#.(setf *with-c-syntax-reader-level* :conservative)
#.(setf *with-c-syntax-reader-case* :preserve)
(defun test-reader-case-sensitivity ()
  (eval-equal nil ()
    #{
    int x \, X \;
    x = 1 \;
    X = 2 \;
    return x == X \;
    }#)
  t)

(defun test-reader ()
  (test-nil-reading)
  
  (test-reader-conservative)
  (test-reader-aggressive)
  (test-reader-overkill)
  (test-reader-insane)

  (test-reader-toplevel-conservative)
  (test-reader-toplevel-aggressive)
  (test-reader-toplevel-overkill)
  (test-reader-toplevel-insane)

  (test-reader-case-sensitivity)
  t)

;;; Agh, I need file-local variable..
#.(setf *with-c-syntax-reader-level* nil)
#.(setf *with-c-syntax-reader-case* nil)
