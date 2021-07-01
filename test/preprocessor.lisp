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

(test test-pp-if-syntax-errors
  ;; TODO: #if
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
  
  ;; TODO: #elif
  
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

(test test-pp-object-like-macro
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
    #if ! defined FUGA
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

(test test-pp-include
  (unwind-protect
       (progn
         (with-open-file (out "/tmp/tmp.h" :direction :output :if-exists :supersede)
           (format out "int hoge = 100;"))
         (let ((form
                 '#2{
                 #include "/tmp/tmp.h"
                 return hoge; // This is with-c-syntax.test::hoge.
                 }#))
           (is (= (let ((*package* (find-package '#:with-c-syntax.test)))
                    (eval form))
                  100))))
    (delete-file "/tmp/tmp.h")))
  
(test test-pp-include-2
  (is.equal.wcs 10
    #2{
    #include <iso646.h>
    return 10 or 9999;
    }#)
  (signals.macroexpand.wcs (with-c-syntax.core::preprocess-include-file-error)
    #2{
    #include <not_exists>
    }#))
  
(test test-pp-strcat
  (is.equal.wcs "abc"
    return "a" "b" "c" \; ))

;;; TODO: add symbol-interning tests
