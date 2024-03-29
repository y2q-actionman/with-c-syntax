(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(test test-pp-error-directive
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

(test test-pp-null-directive
  (let ((str "# and newline immediately after."))
    (is.equal.wcs str
      #2{
      #
      return str \;
      }#))
  (let ((str "#, spaces and newline."))
    (is.equal.wcs str
      #2{
      #  
      return str \;
      }#))
  (let ((str "#, line comment, and newline."))
    (is.equal.wcs str
      #2{
      # // comment
      return str \;
      }#))
  (let ((str "#, block comment, and newline."))
    (is.equal.wcs str
      #2{
      # /* comment */   
      return str \;
      }#))
  (let ((str "#, long block comment, and newline."))
    (is.equal.wcs str
      #2{
      # /* comment
      */   
      return str \;
      }#))
  (let ((str "#, long block comment, line comment, and newline."))
    (is.equal.wcs str
      #2{
      # /* comment
      */// extra comment
      str
      }#)))

(test test-pp-if-syntax-errors
  (signals.macroexpand.wcs ()
    #2{ #if }#)
  (signals.macroexpand.wcs ()
    #2{ #if x y }#)
  
  (signals.macroexpand.wcs ()
    #2{ #ifdef }#)
  (signals.macroexpand.wcs ()
    #2{ #ifdef X Y }#)
  
  (signals.macroexpand.wcs ()
    #2{ #ifndef }#)
  (signals.macroexpand.wcs ()
    #2{ #ifndef X Y }#)
  
  (signals.macroexpand.wcs ()
    #2{ #elif 1 }#)
  (signals.macroexpand.wcs ()
    #2{
    #ifdef X
    #elif  // no-token
    #endif
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #ifdef X
    #else
    #elif 1
    #endif
    }#)
  
  (signals.macroexpand.wcs ()
    #2{ #else }#)
  (signals.macroexpand.wcs ()
    #2{
    #ifdef X
    #else extra-token
    #endif
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #ifdef X
    #else
    #else
    #endif
    }#)
  
  (signals.macroexpand.wcs ()
    #2{ #endif }#)
  (signals.macroexpand.wcs ()
    #2{
    #ifdef X
    #endif extra-token
    }#))

(test test-pp-if-simple-case
  (is.equal.wcs "if-side"
    #2{
    #if 1
    char* x = "if-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "else-side"
    #2{
    #if 0
    char* x = "if-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "else-side"
    #2{
    #if nil
    char* x = "if-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#))

(test test-pp-ifdef
  (is.equal.wcs "ifdef-side"
    #2{
    #define HOGE
    #ifdef HOGE
    char* x = "ifdef-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "else-side"
    #2{
    #undef HOGE
    #ifdef HOGE
    char *x = "ifdef-side";
    #else
    char *x = "else-side";
    #endif
    return x;
    }#))

(test test-pp-ifndef
  (is.equal.wcs "else-side"
    #2{
    #define HOGE
    #ifndef HOGE
    char* x = "ifndef-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "ifndef-side"
    #2{
    #undef HOGE
    #ifndef HOGE
    char *x = "ifndef-side";
    #else
    char *x = "else-side";
    #endif
    return x;
    }#))

(test test-pp-ifdef-simple-nest
  (is.equal.wcs "ifdef HOGE and FUGA"
    #2{
    #define HOGE
    #define FUGA
    
    #ifdef HOGE
    #ifdef FUGA
    char* x = "ifdef HOGE and FUGA";
    #else
    char* x = "ifdef HOGE";
    #endif
    #else
    #ifdef FUGA
    char* x = "ifdef FUGA";
    #else
    char* x = "no definition";
    #endif
    #endif
    
    return x;
    }#)

  (is.equal.wcs "ifdef HOGE"
    #2{
    #define HOGE
    #undef FUGA
    
    #ifdef HOGE
    #ifdef FUGA
    char* x = "ifdef HOGE and FUGA";
    #else
    char* x = "ifdef HOGE";
    #endif
    #else
    #ifdef FUGA
    char* x = "ifdef FUGA";
    #else
    char* x = "no definition";
    #endif
    #endif
    
    return x;
    }#)

  (is.equal.wcs "ifdef FUGA"
    #2{
    #undef HOGE
    #define FUGA
    
    #ifdef HOGE
    #ifdef FUGA
    char* x = "ifdef HOGE and FUGA";
    #else
    char* x = "ifdef HOGE";
    #endif
    #else
    #ifdef FUGA
    char* x = "ifdef FUGA";
    #else
    char* x = "no definition";
    #endif
    #endif
    
    return x;
    }#)

  (is.equal.wcs "no definition"
    #2{
    #undef HOGE
    #undef FUGA
    
    #ifdef HOGE
    #ifdef FUGA
    char* x = "ifdef HOGE and FUGA";
    #else
    char* x = "ifdef HOGE";
    #endif
    #else
    #ifdef FUGA
    char* x = "ifdef FUGA";
    #else
    char* x = "no definition";
    #endif
    #endif
    
    return x;
    }#))

(test test-pp-ifdef-skip-test
  (is.equal.wcs 100
    #2{
    #ifdef FUGA
    #error "this" error is skipped!
    # Unknown Directive is ignored!
    # "Strings" or numbers like
    # 0xfff are also ignored.
    #endif
    
    return 100;
    }#))

(test test-pp-elif
  (is.equal.wcs "ifdef-side"
    #2{
    #define HOGE
    #define FUGA
    #ifdef HOGE
    char* x = "ifdef-side";
    #elif defined FUGA
    char* x = "elif-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "elif-side"
    #2{
    #undef HOGE
    #define FUGA
    #ifdef HOGE
    char* x = "ifdef-side";
    #elif defined FUGA
    char* x = "elif-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  (is.equal.wcs "else-side"
    #2{
    #undef HOGE
    #undef FUGA
    #ifdef HOGE
    char* x = "ifdef-side";
    #elif defined FUGA
    char* x = "elif-side";
    #else
    char* x = "else-side";
    #endif
    return x;
    }#)
  )

(test test-pp-undef-syntax-errors
  (signals.macroexpand.wcs ()
    #2{ #undef }#)
  (signals.macroexpand.wcs ()
    #2{
    #undef X extra-token
    }#))
  
(test test-pp-line-directive
  (is.equal.wcs 100
    #2{
    #line 100
    return __LINE__;
    }#)
  (is.equal.wcs (+ 1 10000 -100)
    #2{ return __LINE__
    #line 10000
    + __LINE__
    #line 100
    - __LINE__;
    }#)
  (is.equal.wcs "filename-xxx"
    #2{
    #line 1 "filename-xxx"
    return __FILE__;
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #line 0
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #line 2147483648
    }#)
  ;; TODO: macro expansion.
  )

(test test-pp-not-directive
  (signals.macroexpand.wcs ()
    #2{
    #define MYDEFINE define
    # MYDEFINE This is not allowed.
    }#))
  
;;; #include

(test test-pp-include
  (with-making-include-file (out "/tmp/tmp.h")
      ((format out "int hoge = 100;"))
    (let ((form
            '#2{
            #include "/tmp/tmp.h"
            return hoge         ; // This is with-c-syntax.test::hoge.
            }#))
      (is (= (let ((*package* (find-package '#:with-c-syntax.test)))
               (eval form))
             100)))))
  
(test test-pp-include-2
  (is.equal.wcs 9999
    #2{
    #include <iso646.h>
    return 10 and 9999;
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-include-file-error)
    #2{
    #include <not_exists>
    }#))
  
(test test-pp-strcat
  (is.equal.wcs "abc"
    return "a" "b" "c" \; ))

;;; Macro expansion

(test test-pp-simple-object-like-macro
  (is.equal.wcs 3
    #2{
    #define HOGE (x + y)
    int x = 1;
    int y = 2;
    return HOGE \;
    }#))

(test test-pp-special-macro
  (is.equal.wcs 1
    #2{ __LINE__ }#)
  (is.equal.wcs 2
    #2{
    __LINE__
    }#)
  (is.equal.wcs 10
    #2{ return __LINE__
    + __LINE__
    + __LINE__
    + __LINE__;
    }#)
  (is.equal.wcs (+ 1 3 7 12 15)
    #2{ return __LINE__
    # // null directive
    + __LINE__
    #ifdef HOGE
    + __LINE__
    #endif
    + __LINE__
    #define FUGA
    #if ! defined(FUGA) && !defined(FUGA) 
    + 999999
    #else
    + __LINE__
    #endif
    #undef FUGA
    + __LINE__;
    }#)
  (is (stringp #2{ __DATE__ }#))
  (is (stringp #2{ __TIME__ }#))
  ;; TODO:
  ;; (load-time-value
  ;;  (cond (*compile-file-pathname*
  ;;         (namestring *compile-file-pathname*))
  ;;        (*load-pathname*
  ;;         (namestring *load-pathname*))
  ;;        (t
  ;;         nil)))
  (is (typep 
       #2{ __FILE__ }#
       '(or string cl:null))))

(test test-pp-predefined-stdc-macro
  (is.equal.wcs 1
    #{ __STDC__ }#)
  (is.equal.wcs "defined!"
    #{
    #if defined __STDC__
    "defined!"
    #else
    #error
    #endif
    }#)
  (is
   #2{
   member (__STDC_HOSTED__, list(0, 1));
   }#)
  (is.equal.wcs 1
    #{ __STDC_MB_MIGHT_NEQ_WC__ }#)
  (is.equal.wcs "OK"
    ;; This case does not run on reader level 1, because of 'L' suffix of the integer.
    #2{
    #if __STDC_VERSION__ >= 199901L
    "OK"
    #else
    #error
    #endif
    }#))

(test test-pp-stringify
  (muffle-unused-code-warning
    (is.equal.wcs "ABC"           ; Because readtable-case is :upcase.
      #2{
      #define STR(x) #x
      STR (abc)
      }#)
    (is.equal.wcs "100"
      #2{
      #define STR(x) #x
      STR (100)
      }#)
    (is.equal.wcs ""
      #2{
      #define STR(x) #x
      STR ()
      }#)
    (is.equal.wcs "EMPTY"
      #2{
      #define STR(x) #x		// not expands the argument.
      #define EMPTY
      STR (EMPTY)
      }#)
    (is.equal.wcs ""
      #2{
      #define STR(x) #x
      #define EXPAND_STR(x) STR(x) 
      #define EMPTY
      EXPAND_STR (EMPTY)
      }#)
    (is.equal.wcs "1.2"
      #2{
      #define STR(x) #x
      #define EXPAND_STR(x) STR(x) 
      #define CAT(x,y) x##y
      EXPAND_STR(CAT(1,.2))
      }#)
    (is.equal.wcs "1. 2"
      #2{
      #define STR(x) #x
      #define EXPAND_STR(x) STR(x) 
      #define CAT(x,y) x##y
      EXPAND_STR(CAT(1 ,. 2))
      }#)
    (is.equal.wcs "1.2"
      #2{
      #define STR(x) #x
      #define EXPAND_STR(x) STR(x) 
      #define CAT3(x,y,z) x##y##z
      EXPAND_STR(CAT3(1 ,. , 2))
      }#)))

(test test-pp-concatenate
  (is.equal.wcs "x+y+z"
    #2{
    #define xyz "x+y+z"
    #define xy "x_y"
    #define yz "y_z"
    #define CAT3(x,y,xy) x##y##xy
    CAT3(x,y,z)
    }#))

(test test-pp-weird-rescan
  ;; From mcpp-2.7.2 cpp-test.html#2.7.6
  (is.wcs.pp.equal
   #2{
   FUNC1 ( x , y )
   }#
   #2{
   #define FUNC1( a, b)    ((a) + (b))
   #define FUNC2( a, b)    FUNC1 OP_LPA a OP_CMA b OP_RPA
   #define OP_LPA          (
   #define OP_RPA          )
   #define OP_CMA          ,

   FUNC2( x, y)   
   }#))

(test test-pp-next-token-inclusion
  ;; From mcpp-2.7.2 cpp-test.html#2.7.6
  (is.equal.wcs 21
    #2{
    int a = 1, b = 20;
    
    #define add( x, y)      ((x) + (y))
    #define head            add(

    return head a, b);
    }#))

(test test-pp-next-token-inclusion-2
  ;; From mcpp-2.7.2 cpp-test.html#2.7.6
  (is.wcs.pp.equal
   #2{
   ((x)+(y));
   }#
   #2{
   #define add( x, y)      ((x) + (y))
   #define sub( x, y)      ((x) - (y))
   #define OP  add
   OP( x, y);
   }#))

(test test-pp-not-function-invocation
  (is.wcs.pp.equal
   #2{
   x
   }#
   #2{
   #define x() (a, b, c)
   x
   }#))

;;; pp-number tests.

(test test-pp-number-and-macro
  (is.wcs.pp.equal
   #2{
   12E+EXP
   }#
   #2{
   #define E 8
   #define EXP 999
   12E+EXP
   }#)
  (is.wcs.pp.equal
   #2{
   12+999
   }#
   #2{
   #define EXP 999
   12+EXP
   }#)
  (is.wcs.pp.equal
   #2{
   0xFFFFp+EXP
   }#
   #2{
   #define EXP 999
   0xFFFFp+EXP
   }#)
  (is.wcs.pp.equal
   #2{
   0xBEEF+999
   }#
   #2{
   #define BEEF 1234
   #define EXP 999
   0xBEEF+EXP
   }#))

;;; digraph tests.

(test test-pp-digraph
  #-with-c-syntax-test-use-compiler-let
  (progn (warn "Test ~A was skipped." 'test-pp-digraph)
         (return-from test-pp-digraph nil))
  #+with-c-syntax-test-use-compiler-let
  (trivial-cltl2:compiler-let
      ((with-c-syntax.core::*with-c-syntax-preprocessor-process-digraph* :no-warn))
    (is.equal.wcs 30
      #2{
      int array<:5:> = <%0,1,2,3,4%>;
      int ret = 0;
      int i;
      for (i = 0;
             i < sizeof (array);
             ++i) <%
        ret += array<:i:> * i;
      %>
      return ret;
      }#)
    (is.equal.wcs "<::><%%>"
      #2{
      #define STR(x) #x
      STR (<:) STR (:>) STR (<%) STR (%>)
      }#)
    (is.equal.wcs "%:"
      #2{
      %:define STR(x) %:x
      STR(%:)
      }#)
    (signals.wcs.reader (error) "%:2{ \"This does not works.\" }%:")
    (is.equal.wcs "%:%:"
      #2{
      %:define CAT(x,y) x %:%: y
      %:define STR(x) %:x
      %:define STR_EXPAND(x) STR(x)
      STR_EXPAND(CAT(%:,%:))
      }#)))

;;; pragma

(test test-pp-unknown-pragma
  (signals.macroexpand.wcs (with-c-syntax-style-warning)
    #2{
    #pragma unknown-pragma
    t
    }#)
  (signals.macroexpand.wcs (with-c-syntax-style-warning)
    #2{
    _Pragma("unknown-pragma") 
    t
    }#))

(test test-pp-stdc-pragma
  (signals.macroexpand.wcs ()
    #2{
    #pragma STDC unknown pragma
    }#)
  (is.equal.wcs t
    #2{
    #pragma STDC FP_CONTRACT DEFAULT
    #pragma STDC FENV_ACCESS DEFAULT
    #pragma STDC CX_LIMITED_RANGE DEFAULT
    t
    }#)
  (is.equal.wcs t
    #2{
    _Pragma("STDC FP_CONTRACT DEFAULT")
    _Pragma ("STDC FENV_ACCESS DEFAULT")
    _Pragma ( "STDC CX_LIMITED_RANGE DEFAULT" )
    t
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #pragma STDC FP_CONTRACT HOGE
    }#)
  ;; TODO: Add ON/OFF test if implemented them.
  )

(defpackage test-with-c-syntax-pragma-package
  (:use :cl)
  (:intern #:foo))

(test test-with-c-syntax-pragma
  (is.equal.wcs 'test-with-c-syntax-pragma-package::foo
    #2{
    #pragma WITH_C_SYNTAX IN_PACKAGE "TEST-WITH-C-SYNTAX-PRAGMA-PACKAGE"
    return quote (foo);
    }#)
  (signals.macroexpand.wcs ()
    #2{
    #pragma WITH_C_SYNTAX IN_PACKAGE
    }#)
  ;; TODO: WITH_C_SYNTAX readtable pragma tests
  )

;;; Examples in C standard.

(test test-pp-6.10.3.3-example
  (is.equal.wcs "X ## Y"    ; Because our readtable-case is `:upcase'.
    #2{
    #define hash_hash # ## #
    #define mkstr(a) # a
    #define in_between(a) mkstr(a)
    #define join(c, d) in_between(c hash_hash d)
    char p[] = join(x, y); // equivalent to
                           // char p[] = "x ## y";
    return p;
    }#))

(test test-pp-6.10.3.5-example-1
  (is.wcs.pp.equal
   #2{
   int table [ 100 ] ;
   }#
   #2{
   #define TABSIZE 100
   int table[TABSIZE];
   }#))

(test test-pp-6.10.3.5-example-2
  (is.equal.wcs 100
    #2{
    #define max(a, b) ((a) > (b) ? (a) : (b))
    max (-1, 100)
    }#))

(test test-pp-6.10.3.5-example-3
  (is.wcs.pp.equal
   #2{
   f(2 * (y+1)) + f(2 * (f(2 * (z[0])))) % f(2 * (0)) + t(1);
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-3.h"
   f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
   }#)
  (is.wcs.pp.equal
   #2{
   f(2 * (2+(3,4)-0,1)) \| f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-3.h"
   // (
   g(x+(3,4)-w) \| h 5) & m
   	 (f)^m(m);
   }#)
  (is.wcs.pp.equal
   #2{
   int i[] = { 1, 23, 4, 5,  };
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-3.h"
   p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };
   }#)
  ;; This test must see readtable-case handlings.
  (is.wcs.pp.equal
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   char c[2][6] = { "hello", "" };
   }#
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   #include "test/test-pp-6.10.3.5-example-3.h"
   char c[2][6] = { str(hello), str() };
   }#))

(test test-pp-6.10.3.5-example-4
  (is.wcs.pp.equal
   #2{
   printf("x1= %d, x2= %s", x1, x2);
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-4.h"
   debug(1, 2);
   }#)
  ;; TODO: FIXME: Handling or '\' char is different between C and Lisp.
  (is.wcs.pp.equal
   ;; #2{
   ;; #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   ;; fputs("strncmp(\"abc\\0d\", \"abc\", '\\4') == 0: @\n",
   ;;       s);
   ;; }#
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   fputs("strncmp(\"abc\0d\", \"abc\", '\4') == 0: @n",
         s);
   }#
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   #include "test/test-pp-6.10.3.5-example-4.h"
   fputs(str(strncmp("abc\0d", "abc", '\4') // this goes away
                    == 0) str(: @\n), s);
   }#)
  ;; This is a problematic example, pointed by MCPP.
  ;; One reason to use `+whitespace-marker+' is for this example.
  ;;   See MCPP's cpp-test.html#2.7.2
  ;; TODO: FIXME: Use #include
  (is.wcs.pp.equal
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   /* #include */ "vers2.h"
   }#
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   #include "test/test-pp-6.10.3.5-example-4.h"
   /* #include */ xstr(INCFILE(2).h)
   }#)
  (is.wcs.pp.equal
   #2{
   "hello";
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-4.h"
   glue(HIGH, LOW);
   }#)
  (is.wcs.pp.equal
   #2{
   "hello, world"
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-4.h"
   xglue(HIGH, LOW)
   }#))

(test test-pp-6.10.3.5-example-5
  (is.wcs.pp.equal
   #2{
   int j[] = { 123, 45, 67, 89,
               10, 11, 12,  };
   }#
   #2{
   #define t(x,y,z) x ## y ## z
   int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),
              t(10,,), t(,11,), t(,,12), t(,,) };
   }#))

(test test-pp-6.10.3.5-example-6
  (is.equal.wcs 0
    #2{
    #define OBJ_LIKE (1-1)
    #define OBJ_LIKE /* white space */ (1-1) /* other */
    
    #define FUNC_LIKE(a) ( a )
    #define FUNC_LIKE( a )( /* note the white space */ \
a /* other stuff on this line */ )

    FUNC_LIKE (OBJ_LIKE)
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #define OBJ_LIKE (1-1)
    #define OBJ_LIKE (0) // different token sequence
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #define OBJ_LIKE (1-1)
    #define OBJ_LIKE (1 - 1) // different white space
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #define FUNC_LIKE(a) ( a )
    #define FUNC_LIKE(b) ( a ) // different parameter usage
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-error)
    #2{
    #define FUNC_LIKE(a) ( a )
    #define FUNC_LIKE(b) ( b ) // different parameter spelling
    }#))

(test test-pp-6.10.3.5-example-7
  (is.wcs.pp.equal
   #2{
   fprintf(stderr,  "Flag" );
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-7.h"
   debug("Flag");
   }#)
  (is.wcs.pp.equal
   #2{
   fprintf(stderr,  "X = %d\n", x );
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-7.h"
   debug("X = %d\n", x);    
   }#)
  (is.wcs.pp.equal
   #2{
   puts( "THE FIRST, SECOND, AND THIRD ITEMS." ); // because of :upcase.
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-7.h"
   showlist(The first, second, and third items.);     
   }#)
  (is.wcs.pp.equal
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   puts( "The first, second, and third items." ); // should be it.
   }#
   #2{
   #pragma WITH_C_SYNTAX IN_WITH_C_SYNTAX_READTABLE preserve
   #include "test/test-pp-6.10.3.5-example-7.h"
   showlist(The first, second, and third items.);     
   }#)
  (is.wcs.pp.equal
   #2{
   // puts() argument is different,because of :upcase.
   ((x>y)?puts("X>Y"):
                    printf("x is %d but y is %d", x, y));
   }#
   #2{
   #include "test/test-pp-6.10.3.5-example-7.h"
   report(x>y, "x is %d but y is %d", x, y);
   }#))

;;; TODO: add symbol-interning tests
