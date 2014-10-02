(in-package :with-c-syntax)

(enable-wcs-reader :mode :conservative)
(defun test-reader-conservative ()
  ;; #!
  (eval-equal 7 ()
    return #!(+ 4 3) \;)
  ;; { and }
  (eval-equal 99 ()
    {return 99 \;})
  ;; [ and ]
  (eval-equal 2 ()
    {
    int hoge[]={0 \, 1 \, 2}\;
    return hoge[2]\;
    })
  t)
(disable-wcs-reader)

(enable-wcs-reader :mode :aggressive)
(defun test-reader-aggressive ()
  ;; comma
  (eval-equal 2 ()
    {
    int hoge[]={0,1,2} \;
    return hoge[2]\;
    })
  ;; '.'
  (eval-equal 3 ()
    {
    struct{int x \;}hoge ={3}\;
    return hoge . x \;
    })
  (eval-equal (cons 1 2) ()
    {
    return #!'(1 . 2) \;
    })
  ;; ':'
  (eval-equal 2 ()
    {
    return 1 ? 2 : 3 \;
    })
  t)
(disable-wcs-reader)

(enable-wcs-reader :mode :overkill)
#[defun test-reader-overkill #[]#
  #| semicolon |#
  #[eval-equal 3 #[]#
     {1;2;return 3;}]#
  #| single quote |#
  #[eval-equal #\a #[]#
     return 'a'\;
  ]#
  #| parens |#
  #[eval-equal "a" #[]#
     return string('a');
  ]#
  t
]#
#!(disable-wcs-reader)

(enable-wcs-reader :mode :crazy)
#[defun test\-reader\-crazy #[\&aux #[x 2]# #[y 3]#]#
  #| comments |#
  #[eval\-equal 6 #[]#
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3 ;
  ]#
  #| ? : |#
  #[eval\-equal 2 #[]#
    return 1?x:y;
  ]#
  #| ~ |#
  #[eval\-equal #!(lognot 2) #[]#
    return ~x;
  ]#
  #| = |#
  #[eval\-equal 2 #[]#
    { int hoge=x; return hoge; }
  ]#
  #| == |#
  #[eval\-equal nil #[]#
    return x==y;
  ]#
  #| * |#
  #[eval\-equal 6 #[]#
    return x*y;
  ]#
  #| *= |#
  #[eval\-equal 6 #[]#
    { int hoge=x; hoge*=y; return hoge; }
  ]#
  #| / |#
  #[eval\-equal #!(/ 2 3) #[]#
    return x/y;
  ]#
  #| /= |#
  #[eval\-equal #!(/ 2 3) #[]#
    { int hoge=x; hoge/=y; return hoge; }
  ]#
  #| % |#
  #[eval\-equal 2 #[]#
    return x%y;
  ]#
  #| %= |#
  #[eval\-equal 2 #[]#
    { int hoge=x; hoge%=y; return hoge; }
  ]#
  #| ^ |#
  #[eval\-equal #!(logxor 2 3) #[]#
    return x^y;
  ]#
  #| ^= |#
  #[eval\-equal #!(logxor 2 3) #[]#
    { int hoge=x; hoge^=y; return hoge; }
  ]#
  #| ! |#
  #[eval\-equal nil #[]#
    return !x;
  ]#
  #| != |#
  #[eval\-equal t #[]#
    return x!=y;
  ]#
  #| & |#
  #[eval\-equal #!(logand 2 3) #[]#
    return x&y;
  ]#
  #| &= |#
  #[eval\-equal #!(logand 2 3) #[]#
    { int hoge=x; hoge&=y; return hoge; }
  ]#
  #| && |#
  #[eval\-equal 3 #[]#
    return x&&y;
  ]#
  #| | |#
  #[eval\-equal #!(logior 2 3) #[]#
    return x|y;
  ]#
  #| |= |#
  #[eval\-equal #!(logior 2 3) #[]#
    { int hoge=x; hoge|=y; return hoge; }
  ]#
  #| || |#
  #[eval\-equal 2 #[]#
    return x||y;
  ]#
  #| + |#
  #[eval\-equal 1 #[]#
    return +1;
  ]#
  #[eval\-equal 5 #[]#
    return x+y;
  ]#
  #| += |#
  #[eval\-equal 5 #[]#
    { int hoge=x; hoge+=y; return hoge; }
  ]#
  #| - |#
  #[eval\-equal #!-1 #[]#
    return -1;
  ]#
  #[eval\-equal #!-1 #[]#
    return x-y;
  ]#
  #| -= |#
  #[eval\-equal #!-1 #[]#
    { int hoge=x; hoge-=y; return hoge; }
  ]#
  #| < |#
  #[eval\-equal t #[]#
    return x<y;
  ]#
  #| <= |#
  #[eval\-equal t #[]#
    return x<=y;
  ]#
  #| << |#
  #[eval\-equal #!(ash 2 3) #[]#
    return x<<y;
  ]#
  #| <<= |#
  #[eval\-equal #!(ash 2 3) #[]#
    { int hoge=x; hoge<<=y; return hoge; }
  ]#
  #| > |#
  #[eval\-equal nil #[]#
    return x>y;
  ]#
  #| >= |#
  #[eval\-equal nil #[]#
    return x>=y;
  ]#
  #| >> |#
  #[eval\-equal #!(ash 2 -3) #[]#
    return x>>y;
  ]#
  #| >>= |#
  #[eval\-equal #!(ash 2 -3) #[]#
    { int hoge=x; hoge>>=y; return hoge; }
  ]#

  #| . |#
  #[eval\-equal 3 #[]#
    {
    struct{int x;}hoge={3};
    return hoge.x;
    }
  ]#
  #| -> |#
  #[eval\-equal 3 #[]#
    {
    struct{int x;}hoge={3};
    return (&hoge)->x;
    }
  ]#
  t
]#
#!(disable-wcs-reader)

(defun test-reader ()
  (test-reader-conservative)
  (test-reader-aggressive)
  (test-reader-overkill)
  (test-reader-crazy)
  t)
