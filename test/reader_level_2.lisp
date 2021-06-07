(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

;;; Translation phase 1 ~ 3

(test test-reader-trigraph
  (is.equal.wcs "#[\\]^{|}~"
    #2{
    return "??=??(??/??/??)??'??<??!??>??-";
    }#)
  (is.equal.wcs "Eh?
"
    ;; from 5.2.1.1 example
    #2{
    return "Eh???/n";
    }#)
  (is.equal.wcs (logand (lognot (logior (logxor 1 3) 4)) #xFFFF)
    #2{
    int array??(??)=??<1,3??>;
    int x = 0;
    x=array??(0??)??'array??(1??); // x = array[0] ^ array[1];
    x??!=0x4; // x |= 0x4;
    x=??-x; // x ~= x;
    x&=0xFFFF;
    return x;
    }#)
  (is.equal.wcs 99
    ;; '#{' can be closed with trigraph.
    ;; (This feature is not intentional at first, but I thought it should be work at last.)
    #2{ return 99; ??>??=
    ))

(test test-reader-backslash-newline
  ;; Inside keywords, integer constants, strings.
  (is.equal.wcs (format nil "116abcde~C" #\tab) 
    #2{
    // ; int foo = 100;
    i\
nt foo = 1\
0\
0;
    // ; const char bar[] = "abcde\t";
    const ch\
ar b\
ar[] = "ab\
cd\
e\\
t";

    // ; foo += 0x010;
    fo\
o +\
= 0\
x10\
;	

    // ; return format (nil, "~D~A", foo, bar);
    ret\
urn form\
at (n\
i\
l, "~\
D~A", foo\
, ba\
r);
    }#)
  ;; backslash-newline and comment syntax.
  (is.equal.wcs t
    #2{
    // ; This Line is comment \
    also, this line is a comment!!!
    return t;
    }#)
  (is.equal.wcs t
    #2{
    /\
/ This Line is comment 
    return t;
    }#)
  (is.equal.wcs t
    #2{
    /\
* This block is comment.
    == comment == \
    return 114514;
    *\
/
    return t;
    }#)
  ;; trigraph can generate '\'
  (is.equal.wcs "abcde"
    #2{
    return "abc??/
de";
    }#)
  ;; Trigraph and backslash-newline and comment
  (is.equal.wcs "many comments.."
    #2{
    /??/
* This is a comment *??/
/
    // This is also comment ??/
    This line is also comment.
    return "many comments..";
    }#)
  ;; It works between '}#'.
  (is.equal.wcs 0
    #2{
    return 0;
    }\
#  ))

(test test-reader-comments-and-backquote
  (is (block nil
        #2{
        // `(return nil) }# ; In C comment, '`' and '}#' does not works. 
        return t;
        }#))
  (is #2{
      return
      `(progn
         ;; `(,(error "???")) }#  In Lisp comment, }# does not works.
         t);
      }#)
  (is (= 10
       (block nil
         #2{
         // `(return nil) ; not works.
         `(progn
            ;; }# does not works.
            #2{
            // `(return nil) ; not works.
            return
            `(progn
               (+
                #| }# still not works. |#
                1
                #{ return `2; }#
                #2{ return 3 // + 810
                + /* `(return 114514) */ 4;
                }#
                ));
            // }# `(return 'broken!)
            }# #| }|#;# `(return 'broken!)
            );
         // `(return nil) ; not works.
         }#))))

;;; Tokenization

(test test-reader-overkill
 (let ((x 2) (y 3))
  ;; single quote
  (is.equal.wcs #\a
    #2{
    return 'a'\;
    }#)
  (is.equal.wcs (code-char #xa0)
    #2{
    return '\u00A0'\;
    }#)
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
    extern int dummy_for_slime; return x|y;
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
    }#)))

(test test-reader-overkill-default-level
  #.(setf *with-c-syntax-reader-level* 2)
  (is.equal.wcs 6
    #{
    return 1 // + 8000
      + 2 /* + 9999 */
      + 3;
    }#)
  #.(setf *with-c-syntax-reader-level* with-c-syntax.core::+with-c-syntax-default-reader-level+))

(test test-reader-overkill-2
  ;; Some numeric operator combinations.
  (is.equal.wcs t
    #2{
    return 1+2+3-4+5+6+78+9==100&&1*2*3*4+5+6+7*8+9==100
    &&1*2*3-4*5+6*7+8*9==100&&-1+2-3+4+5+6+78+9==100
    &&98-76+54+3+21==100&&98+7-6*5+4*3*2+1==100;
    }#)
  (is.equal.wcs t
    #2{
    int x=96+1428/357;
    return x==3+69258/714;
    }#))

;;; Constants

(test test-reader-integer
  (is.equal.wcs 0
    #2{
    return 0;
    }#)
  ;; decimal integers
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
    return 0X10;
    }#)
  (is.equal.wcs #xdeadbeef
    #2{
    return 0xDeadBeef;
    }#)
  ;; binary integers
  (is.equal.wcs #b0
    #2{
    return 0b00;
    }#)
  (is.equal.wcs #b10
    #2{
    return 0b10;
    }#))

(test test-reader-bad-char-literal
  (signals.wcs.reader (end-of-file) "#2{ return '")
  (signals.wcs.reader () "#2{ return ''; }#")
  (signals.wcs.reader () "#2{ return '
'; }#")
  (signals.wcs.reader () "#2{ return '; }#")
  (signals.wcs.reader () "#2{ return '\\'; }#")
  (signals.wcs.reader () "#2{ return '\\Y'; }#")
  (signals.wcs.reader () "#2{ return '\\x; }#")
  (signals.wcs.reader () "#2{ return '\\u; }#")
  (signals.wcs.reader () "#2{ return '\\u12; }#")
  (signals.wcs.reader () "#2{ return '\\U1234; }#")
  (signals.wcs.reader () "#2{ return '\\u0060; }#")
  (signals.wcs.reader () "#2{ return '\\U0000D800; }#")
  (signals.wcs.reader () "#2{ return '\\uDFFF; }#")
  (signals.wcs.reader () "#2{ return '\\99'; }#")
  ;; Below will make a two-character integer constant (\022 and 3).
  ;; See "6.4.4.4 Character constants" 14 Example 3.
  (signals.wcs.reader () "#2{ return '\\0223; }#"))

(test test-reader-integer-bad-character
  (signals.wcs.reader () "#2{ return 1ff; }#")
  (signals.wcs.reader () "#2{ return 08; }#")
  (signals.wcs.reader () "#2{ return 0xgg; }#")
  (signals.wcs.reader () "#2{ return 0b22; }#"))

(test test-reader-integer-suffix
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
    }#))

(test test-reader-integer-bad-suffix
  (signals.wcs.reader () "#2{ return 0uu; }#")
  (signals.wcs.reader () "#2{ return 0lll; }#")
  (signals.wcs.reader () "#2{ return 1xx; }#")
  (signals.wcs.reader () "#2{ return 0lL; }#")
  (signals.wcs.reader () "#2{ return 0Ll; }#")
  (signals.wcs.reader () "#2{ return 0Lul; }#")
  (signals.wcs.reader () "#2{ return 0uuL; }#"))

(test test-reader-decimal-float
  (is.equal.wcs 1d0
    #2{
    return 1.0;
    }#)
  (is.equal.wcs 2d0
    #2{
    return 2.;
    }#)
  (is.equal.wcs 0.3f0
    #2{
    return .3f;
    }#)
  (is.equal.wcs 810d0
    #2{
    return 810e+0;
    }#)
  (is.equal.wcs 2112L-93
    #2{
    return 2112E-93L;
    }#)
  (is.equal.wcs 0d0
    #2{
    return 0.;
    }#)
  (is.equal.wcs 0d0
    #2{
    return .0;
    }#)
  (is.equal.wcs 70d0
    #2{
    return 070.;
    }#)
  (signals.wcs.reader () "#2{ 0..0; }#")
  (signals.wcs.reader () "#2{ 0.0.0; }#")
  (is.equal.wcs 1.3d0
    #2{
    return 1.3e0;
    }#)
  (is.equal.wcs 4.2d-10
    #2{
    return 4.2E-10;
    }#)
  (is.equal.wcs -0.11f+2
    #2{
    return -.11E+2f;
    }#)
  (is.equal.wcs 114.51L+4
    #2{
    return 114.51E+4L;
    }#)
  (signals.wcs.reader () "#2{ 0.0fl; }#")
  (signals.wcs.reader () "#2{ 1f; }#")
  (signals.wcs.reader () "#2{ -0f; }#")
  (signals.wcs.reader () "#2{ 0.e; }#")
  (signals.wcs.reader () "#2{ .1e+; }#")
  (signals.wcs.reader () "#2{ .1eF; }#")

  ;; Identifiers consists of decimal float chars.
  (is.equal.wcs 100d0
    #2{
    double e0 = 100e0;
    return e0;
    }#)
  (is.equal.wcs 1L1
    #2{
    long double e0FL = 1e+1l;
    return e0FL;
    }#)

  ;; Dot operator and identifier
  (is.equal.wcs (+ 42d1 2f0)
    #2{
    struct hoge {
       double e1;
    } xxx;
    xxx.e1 = 42.e1;
    return xxx.e1 + 2.f;
    }#)
  (is.equal.wcs (- 3L-3 3)
    #2{
    struct hoge {
       double E3;
    } xxx;
    xxx.E3 = 3E-3L-3L;
    return xxx.E3;
    }#)
  (is.equal.wcs (+ 42 0d1 0.1f-1 0.1f0 1)
    #2{
    struct hoge {
       int e;
    } f = {42};
    return f.e+0.e+1+.1e-1f+.1f+1;
    }#))

(test test-reader-hexadecimal-float
  (signals.wcs.reader () "#2{ 0x0.0; }#")
  (is.equal.wcs 0d0
    #2{
    return 0x0.0p0;
    }#)
  (signals.wcs.reader () "#2{ 0x2.; }#")
  (is.equal.wcs (* 2d0 (expt 2 1))
    #2{
    return 0x2.p1;
    }#)
  (signals.wcs.reader () "#2{ 0x.3f; }#")
  (signals.wcs.reader () "#2{ .3p0f; }#")
  (is.equal.wcs 0.1875f0
    #2{
    return 0x.3p0f;
    }#)
  (signals.wcs.reader () "#2{ 0x810e+0; }#")
  (is.equal.wcs (float #x810 0d0)
    #2{
    return 0x810p+0;
    }#)
  (signals.wcs.reader () "#2{ 0x2112E-93L; }#")
  (is.equal.wcs (* 8466l0 (expt 2 -93))
    #2{
    return 0x2112P-93L;
    }#)
  (signals.wcs.reader () "#2{ 0x0.; }#")
  (is.equal.wcs 0d0
    #2{
    return 0x0.p0;
    }#)
  (signals.wcs.reader () "#2{ 0x.0; }#")
  (is.equal.wcs 0d0
    #2{
    return 0x.0p0;
    }#)
  (signals.wcs.reader () "#2{ 0x..p0; }#")
  (signals.wcs.reader () "#2{ 0x.0.p0; }#")
  (signals.wcs.reader () "#2{ 0x1.3e0; }#")
  (is.equal.wcs 1.1875d0
    #2{
    return 0x1.3p0;
    }#)
  (signals.wcs.reader () "#2{ 0x4.2E-10; }#")
  (is.equal.wcs (* 4.125d0 (expt 2 -10))
    #2{
    return 0x4.2P-10;
    }#)
  (signals.wcs.reader () "#2{ -0x.11E+2f; }#")
  (is.equal.wcs (* -0.0625f0 (expt 2 2))
    #2{
    return -0x.1P+2f;
    }#)
  (signals.wcs.reader () "#2{ -0x114.5E+14L; }#")
  (is.equal.wcs (* 276.3125L0 (expt 2 14))
    #2{
    return 0x114.5P+14L;
    }#)
  (signals.wcs.reader () "#2{ 0x0.0p0fl; }#")
  (signals.wcs.reader () "#2{ 0x0.p; }#")
  (signals.wcs.reader () "#2{ 0x.1p+; }#")
  (signals.wcs.reader () "#2{ 0x.1pF; }#")

  ;; Integers consists of hexadicimal float chars.
  (is.equal.wcs #x0e0
    #2{
    return 0x0e0;
    }#)
  (is.equal.wcs #x0e0f
    #2{
    return 0x0e0f;
    }#)

  ;; Identifiers consists of hexadecimal float chars.
  (is.equal.wcs (float #x100 0d0)
    #2{
    double p0 = 0x100p0;
    return p0;
    }#)
  (is.equal.wcs (* (float #x1 0d0) (expt 2 1))
    #2{
    long double p0FL = 0x1p+1l;
    return p0FL;
    }#)

  ;; Dot operator and identifier
  (is.equal.wcs (* (float #x42 0d0) (expt 2 1))
    #2{
    struct hoge {
       double p1;
    } xxx;
    xxx.p1 = 0x42.p1;
    return xxx.p1;
    }#)
  (signals.wcs.reader () "#2{ 0x3E-3L; }#")
  (signals.wcs.reader () "#2{ 0x3E- 3L; }#")
  (is.equal.wcs (- #x3E 3 (* (float #x3 0d0) (expt 2 -3)))
    #2{
    struct hoge {
       double P3;
    } xxx;
    xxx.P3 = 0x3E -3L-0x3.P-3L;
    return xxx.P3;
    }#)
  (is.equal.wcs (+ 42 0d1 (* 0.0625f0 (expt 2 -1)) 0.0625f0 1)
    #2{
    struct hoge {
       int p;
    } f = {42};
    return f.p+0x0.p+1+0x.1p-1f+0x.1p0f+1;
    }#))

(test test-reader-minus-number
  ;; minus operator (Prefix '-' is not a part of C numeric literal.)
  (is.equal.wcs -0
    #2{
    return -0;
    }#)
  (is.equal.wcs -0
    #2{
    return -0L;
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
  (is.equal.wcs #b-101
    #2{
    return -0b101LL;
    }#)
  (is.equal.wcs #o-777
    #2{
    return -00777;
    }#)
  (is.equal.wcs -0d0
    #2{
    return -0e0;
    }#)
  (is.equal.wcs -0d0
    #2{
    return -0x0p0;
    }#)
  ;; minus exponents
  (is.equal.wcs -1d-1
    #2{
    return -1e-1L;
    }#)
  (signals.wcs.reader () "#2{ return -1e -1L; }#")
  (is.equal.wcs (* -1d0 (expt 2 -1))
    #2{
    return -0x1p-1L;
    }#)
  (signals.wcs.reader () "#2{ return -0x1p -1L; }#"))
