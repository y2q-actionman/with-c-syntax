(in-package :with-c-syntax)

(use-reader :level :conservative)
(defun test-reader-conservative ()
  ;; #!
  (eval-equal 7 ()
    #0{
    return #!(+ 4 3) \;
    }#)
  ;; comma
  (eval-equal 2 ()
    #0{
    {
    int hoge-array [ ] = { 0,1,2 } \;
    return hoge-array [ 2 ] \;
    }
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
(unuse-reader)

(use-reader :level :aggressive)
(defun test-reader-aggressive ()
  ;; { and }
  (eval-equal 99 ()
    #1{{return 99 \;}}#)
  ;; [ and ]
  (eval-equal 2 ()
    #1{
    {
    int hoge-array[]={0,1,2}\;
    return hoge-array[2]\;
    }
    }#)
  ;; '.'
  (eval-equal 3 ()
    #1{
    {
    struct{int x \;}hoge ={3}\;
    return hoge . x \;
    }
    }#)
  (eval-equal (cons 1 2) ()
    #1{
    {
    return #!'(1 . 2) \;
    }
    }#)
  ;; check default-level
  (eval-equal 99 ()
    #{{return 99 \;}}#)
  t)
(unuse-reader)

(use-reader :level :overkill)
(defun test-reader-overkill ()
  #| semicolon |#
  (eval-equal 3 ()
    #2{{1;2;return 3;}}#
    )
  #| single quote |#
  (eval-equal #\a ()
    #2{
    return 'a'\;
    }#)
  #| parens |#
  (eval-equal "a" ()
    #2{
    return string('a');
    }#)
  ;; check default-level
  (eval-equal 3 ()
    #{{1;2;return 3;}}#
    )
  t)
(unuse-reader)

(use-reader :level :insane)
(defun test-reader-insane (&aux (x 2) (y 3))
  #| comments |#
  (eval-equal 6 ()
    #3{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  #| ? : |#
  (eval-equal 2 ()
    #3{
    return 1?x:y;
    }#)
  #| ~ |#
  (eval-equal (lognot 2) ()
    #3{
    return ~x;
    }#)
  #| = |#
  (eval-equal 2 ()
    #3{
    { int hoge=x; return hoge; }
    }#)
  #| == |#
  (eval-equal nil ()
    #3{
    return x==y;
    }#)
  #| * |#
  (eval-equal 6 ()
    #3{
    return x*y;
    }#)
  #| *= |#
  (eval-equal 6 ()
    #3{
    { int hoge=x; hoge*=y; return hoge; }
    }#)
  #| / |#
  (eval-equal (/ 2 3) ()
    #3{
    return x/y;
    }#)
  #| /= |#
  (eval-equal (/ 2 3) ()
    #3{
    { int hoge=x; hoge/=y; return hoge; }
    }#)
  #| % |#
  (eval-equal 2 ()
    #3{
    return x%y;
    }#)
  #| %= |#
  (eval-equal 2 ()
    #3{
    { int hoge=x; hoge%=y; return hoge; }
    }#)
  #| ^ |#
  (eval-equal (logxor 2 3) ()
    #3{
    return x^y;
    }#)
  #| ^= |#
  (eval-equal (logxor 2 3) ()
    #3{
    { int hoge=x; hoge^=y; return hoge; }
    }#)
  #| ! |#
  (eval-equal nil ()
    #3{
    return !x;
    }#)
  #| != |#
  (eval-equal t ()
    #3{
    return x!=y;
    }#)
  #| & |#
  (eval-equal (logand 2 3) ()
    #3{
    return x&y;
    }#)
  #| &= |#
  (eval-equal (logand 2 3) ()
    #3{
    { int hoge=x; hoge&=y; return hoge; }
    }#)
  #| && |#
  (eval-equal 3 ()
    #3{
    return x&&y;
    }#)
  #| | |#
  (eval-equal (logior 2 3) ()
    #3{
    return x|y;
    }#)
  #| |= |#
  (eval-equal (logior 2 3) ()
    #3{
    { int hoge=x; hoge|=y; return hoge; }
    }#)
  #| || |#
  (eval-equal 2 ()
    #3{
    return x||y;
    }#)
  #| + |#
  (eval-equal 1 ()
    #3{
    return +1;
    }#)
  (eval-equal 5 ()
    #3{
    return x+y;
    }#)
  #| += |#
  (eval-equal 5 ()
    #3{
    { int hoge=x; hoge+=y; return hoge; }
    }#)
  #| - |#
  (eval-equal -1 ()
    #3{
    return -1;
    }#)
  (eval-equal -1 ()
    #3{
    return x-y;
    }#)
  #| -= |#
  (eval-equal -1 ()
    #3{
    { int hoge=x; hoge-=y; return hoge; }
    }#)
  #| < |#
  (eval-equal t ()
    #3{
    return x<y;
    }#)
  #| <= |#
  (eval-equal t ()
    #3{
    return x<=y;
    }#)
  #| << |#
  (eval-equal (ash 2 3) ()
    #3{
    return x<<y;
    }#)
  #| <<= |#
  (eval-equal (ash 2 3) ()
    #3{
    { int hoge=x; hoge<<=y; return hoge; }
    }#)
  #| > |#
  (eval-equal nil ()
    #3{
    return x>y;
    }#)
  #| >= |#
  (eval-equal nil ()
    #3{
    return x>=y;
    }#)
  #| >> |#
  (eval-equal (ash 2 -3) ()
    #3{
    return x>>y;
    }#)
  #| >>= |#
  (eval-equal (ash 2 -3) ()
    #3{
    { int hoge=x; hoge>>=y; return hoge; }
    }#)

  #| . |#
  (eval-equal 3 ()
    #3{
    {
    struct{int x;}hoge={3};
    return hoge.x;
    }
    }#)
  #| -> |#
  (eval-equal 3 ()
    #3{
    {
    struct{int x;}hoge={3};
    return (&hoge)->x;
    }
    }#)

  ;; check default-level
  (eval-equal 6 ()
    #{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  t)
(unuse-reader)

(use-reader :level :insane)
#{ progn
#[defun test\-reader\-insane\-toplevel #[]#
  #[eval\-equal #!(+ 1 (* 2 3) (- 4)) #[]#
     return 1+2*3-4;
  ]#
  #[eval\-equal 2 #[]#
   #1{
   {
   int hoge-array[]={0,1,2}\;
   return hoge-array[2]\;
   }
   }#]#
  t]#
}#
(unuse-reader)

(defun test-reader ()
  (test-reader-conservative)
  (test-reader-aggressive)
  (test-reader-overkill)
  (test-reader-insane)
  (test-reader-insane-toplevel)
  t)
