(in-package #:with-c-syntax.test)

;;; declarations

(test test-decl-simple
  (is.equal.wcs nil
    int *xxx* \;)
  (is (boundp '*xxx*))
  (is.equal.wcs 1
    { int a \; a = 1 \; return a \; })
  (is.equal.wcs nil
    { int a \; \( void \) a \; })
  (is.equal.wcs nil
    { int \; })             ; should be warned?
  (is.equal.wcs 0	    ; returns b's init value.
    { int \; int a \; int \; int b \; \( void \) a \, b \; }))

(test test-decl-list
  (is.equal.wcs 1
    { int a = 1 \; return a \; })
  (is.equal.wcs 2
    { int a = 1 \; int b = 2 \; return a \, b \; })
  (is.equal.wcs 3
    { int a = 1 \; int b = 2 \; int c = 3 \; return a \, b \, c \; }))

;; TODO: add 'global' decl -- into translation-unit tests.
;; TODO: add 'static' decl's extent test.
;; TODO: add typedef
(test test-decl-specs
  ;; storage-class
  (is.equal.wcs 1
    { int x = 1 \; return x \; })
  (is.equal.wcs 2
    { auto int x = 2 \; return x \; })
  (is.equal.wcs 3
    { register int x = 3 \; return x \; })
  (is.equal.wcs 4
    { static int x = 4 \; return x \; })
  (let ((x 5))
    (is.equal.wcs 5
      { extern int x \; return x \; }))
  (signals.macroexpand.wcs ()
    { extern int x = 999 \; })
  (is.equal.wcs nil
    { typedef int \; })
  (signals.macroexpand.wcs ()
    { typedef int x = 999 \; })
  (signals.macroexpand.wcs ()
    { auto auto auto int \; })
  (signals.macroexpand.wcs ()
    { auto register int \; })

  (is.equal.wcs 6
    { auto x = 6 \; return x \; })
  (is.equal.wcs 7
    { register x = 7 \; return x \; })
  (is.equal.wcs 8
    { static x = 8 \; return x \; })
  (let ((x 9))
    (is.equal.wcs 9
      { extern x \; return x \; }))
  (signals.macroexpand.wcs ()
    { extern x = 999 \; })
  (is.equal.wcs nil
    { typedef x  \; })
  (signals.macroexpand.wcs ()
    { typedef x = 999 \; })
  (signals.macroexpand.wcs ()
    { auto auto \; })
  (signals.macroexpand.wcs ()
    { auto register \; })

  (is.equal.wcs nil
    int func \( \) \;)
  (signals.macroexpand.wcs ()
    auto int func \( \) \;)
  (signals.macroexpand.wcs ()
    register int func \( \) \;)
  (is.equal.wcs nil
    static int func \( \) \;)
  (is.equal.wcs nil
    extern int func \( \) \;)
  (signals.macroexpand.wcs ()
    typedef int func \( \) \;)

  ;; type-spec
  (signals.macroexpand.wcs ()
    { void x \; })

  (is.equal.wcs 10
    { int x = 10 \; return x \; })
  (is.equal.wcs 11
    { signed int x = 11 \; return x \; })
  (is.equal.wcs 11
    { unsigned int x = 11 \; return x \; })
  (signals.macroexpand.wcs ()
    { signed signed x \; })
  (signals.macroexpand.wcs ()
    { signed unsigned x \; })
  (signals.macroexpand.wcs ()
    { unsigned unsigned x \; })
  (is.equal.wcs 12
    { short int x = 12 \; return x \; })
  (is.equal.wcs 13
    { short signed int x = 13 \; return x \; })
  (is.equal.wcs 14
    { short unsigned int x = 14 \; return x \; })
  (signals.macroexpand.wcs ()
    { short short x \; })
  (is.equal.wcs 15
    { long int x = 15 \; return x \; })
  (is.equal.wcs 16
    { signed long int x = 16 \; return x \; })
  (is.equal.wcs 17
    { unsigned long int x = 17 \; return x \; })
  (is.equal.wcs 18
    { long long int x = 18 \; return x \; })
  (is.equal.wcs 19
    { signed long long int x = 19 \; return x \; })
  (is.equal.wcs 20
    { unsigned long long int x = 20 \; return x \; })
  (signals.macroexpand.wcs ()
    { long long long \; })

  (is.equal.wcs 21
    { char x = 21 \; return x \; })
  (is.equal.wcs 22
    { signed char x = 22 \; return x \; })
  (is.equal.wcs 23
    { char unsigned x = 23 \; return x \; })
  (signals.macroexpand.wcs ()
    { short char \; })
  (signals.macroexpand.wcs ()
    { long char \; })

  (is.equal.wcs 1f1
    { float x = 1f1 \; return x \; })
  (is.equal.wcs 2s2			; This is our extension.
    { short float x = 2s2 \; return x \; })
  (signals.macroexpand.wcs ()
    { long float \; })
  (signals.macroexpand.wcs ()
    { signed float \; })
  (signals.macroexpand.wcs ()
    { unsigned float \; })

  (is.equal.wcs 3d3
    { double x = 3d3 \; return x \; })
  (is.equal.wcs 4l4
    { long double x = 4l4 \; return x \; })
  (signals.macroexpand.wcs ()
    { short double \; })
  (signals.macroexpand.wcs ()
    { signed double \; })
  (signals.macroexpand.wcs ()
    { unsigned double \; })

  ;; cv-qualifier
  (is.equal.wcs 30
    { const int x = 30 \; return x \; })
  (is.equal.wcs 31
    { int const x = 31 \; return x \; })
  (is.equal.wcs 32
    { const x = 32 \; return x \; })
  ;; TODO: support this?
  ;; (signals.macroexpand.wcs ()
  ;;   { int const x = 0 \; x = 1 \; })

  (is.equal.wcs 33
    { volatile int x = 33 \; return x \; })
  (is.equal.wcs 34
    { int volatile x = 34 \; return x \; })
  (is.equal.wcs 35
    { volatile x = 35 \; return x \; })

  (is.equal.wcs 36
    { const volatile register signed short int x = 36 \; return x \; })

  (signals.macroexpand.wcs ()
    { const auto unsigned int float \; }))

(test test-struct-or-union-spec
  (signals.macroexpand.wcs ()
    { struct \; })
  (signals.macroexpand.wcs ()
    { union \; })
  (is.equal.wcs 100
    {
    struct hoge { int x \; } foo \;
    foo \. x = 100 \;
    return foo \. x \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \; int y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \; int y \; int z \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (is.equal.wcs 1
    {
    struct { int x \; } foo \;
    foo \. x = 1 \;
    return foo \. x \;
    })
  (is.equal.wcs t
    {
    struct { int x \; int y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (is.equal.wcs t
    {
    struct a \;
    struct a * p \;
    \( void \) p \;
    return t \;
    })
  (is.equal.wcs t
    {
    union a \;
    union a * p \;
    \( void \) p \;
    return t \;
    })
  (is.equal.wcs 100
    {
    union hoge { int x \; int y \; } foo \;
    foo \. x = 100 \;
    return foo \. x \;
    }))

(test test-init-declarator-list
  (is.equal.wcs t
    { int a \; \( void \) a \; return t \; })
  (is.equal.wcs t
    { int a \, b \; \( void \) a \, b \; return t \; })
  (is.equal.wcs t
    { int a \, b \, c \; \( void \) a \, b \, c \; return t \; })
  (is.equal.wcs 0
    { int a = 0 \; return a \; })
  (is.equal.wcs 1
    { int a \, b = 1 \; \( void \) a \; return b \; })
  (is.equal.wcs t
    { int a \, b = 1 \, c = 2 \; \( void \) a \; return b == 1 && c == 2 \; }))

;; TODO: add cv-qualifier tests!
(test test-spec-qualifier-list
  (is.equal.wcs 100
    {
    struct hoge { int x \; } foo = { 100 } \;
    return foo \. x \;
    })
  (is.equal.wcs 101
    {
    struct hoge { unsigned x \; } foo = { 101 } \;
    return foo \. x \;
    })

  (is.equal.wcs 100
    {
    struct hoge { const x \; } foo = { 100 } \;
    return foo \. x \;
    })
  ;; TODO: support const
  #+ignore
  (signals.wcs ()
    {
    struct hoge { const x \; } foo \;
    foo \. x = 99 \;
    })

  (is.equal.wcs 200
    {
    struct hoge { volatile x \; } foo \;
    foo \. x = 200 \;
    return foo \. x \;
    })

  (is.equal.wcs 300
    {
    struct hoge { const unsigned volatile int x \; } foo = { 300 } \;
    return foo \. x \;
    })
  ;; TODO: support const
  #+ignore
  (signals.wcs ()
    {
    struct hoge { const unsigned volatile int x \; } foo = { 300 } \;
    foo \. x = 9999 \;
    })
  )

(test test-struct-declarator
  (is.equal.wcs t
    { struct hoge { int x \; } foo \; \( void \) foo \; return t \; })
  (is.equal.wcs t
    {
    struct hoge { int x \, y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \, y \, z \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \: 3 \; } foo \;
    foo \. x = 1 \;
    return foo \. x == 1 \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \, y \: 3 \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \: 1 \, y \: 5 \, z \: 8 \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (signals.macroexpand.wcs ()
    { struct hoge { int x \: 99 \; } \; }))

(test test-enum-spec
  (is.equal.wcs 100
    {
    enum hoge \;
    enum hoge foo = 100 \;
    return foo \;
    })
  (is.equal.wcs t
    {
    enum hoge { x \, y = 4 \, z } \;
    return x == 0 && y == 4 && z == 5 \;
    })
  (is.equal.wcs t
    {
    enum { x = 0 \, y \, z = 3 } \;
    return x == 0 && y == 1 && z == 3 \;
    }))


(test test-param-type-list
  ;; NOTE: These 'sizeof' test only checks these notations can be parsed or not..
  (is.equal.wcs 1
    { return sizeof \( int \( int \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( unsigned int \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( int * \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( int * * \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( const unsigned int * hoge \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( const struct hogehoge * * \) \) \; }))

(test test-type-name
  ;; uses cast
  (is.equal.wcs 5
    { unsigned x = 5 \; return \( int \) x \; })
  (is.equal.wcs 8
    { int x = 8 \; return \( unsigned int \) x \; })

  ;; NOTE: These 'sizeof' test only checks these notations can be parsed or not..

  ;; abstract-declarator -- pointer
  (is.equal.wcs 1
    { return sizeof \( int * \) \; })
  (is.equal.wcs 1
    { return sizeof \( int const * \) \; }) 	; not included in pointer..
  (is.equal.wcs 1
    { return sizeof \( int * const \) \; })
  (is.equal.wcs 1
    { return sizeof \( int * * \) \; })
  (is.equal.wcs 1
    { return sizeof \( int * const * const * \) \; })

  ;; abstract-declarator -- direct-abstract-declarator
  (is.equal.wcs 1
    { return sizeof \( int \( * \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( * \( int \) \) \) \; })

  (is.equal.wcs 2
    { return sizeof \( int [ 1 ] [ 2 ] \) \; })
  (is.equal.wcs 6
    { return sizeof \( int [ 1 ] [ 2 ] [ 3 ] \) \; })
  (is.equal.wcs 5
    { return sizeof \( int [ 5 ] \) \; })

  (signals.macroexpand.wcs ()		; empty dims
    { return sizeof \( int [ ] \) \; })
  (signals.macroexpand.wcs ()		; empty dims
    { return sizeof \( int [ ] [ ] [ ] \) \; })

  (signals.macroexpand.wcs ()		; array of funcs
    { return sizeof \( int [ ] \( int \) \) \; })

  ;; 'sizeof' to function. (It is a weird extension..)
  (is.equal.wcs 1			; using param-type-list
    { return sizeof \( int \( int \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( int \, |...| \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( int \, int \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int \( int \, int \, const int \) \) \; })

  (signals.macroexpand.wcs ()		; array of funcs
    { return sizeof \( int [ ] \( \) \) \; })
  (signals.macroexpand.wcs ()		; func returns a func
    { return sizeof \( int \( \) \( \) \) \; })
  (is.equal.wcs 1			; K&R-style func
    { return sizeof \( int \( \) \) \; })

  ;; abstract-declarator
  (is.equal.wcs 1
    { return sizeof \( int * \( * \) \) \; })
  (is.equal.wcs 1
    { return sizeof \( int * \( * \( int \) \) \) \; }))


(test test-declarator ()
  ;; uses init-declarator

  ;; NOTE: These tests only checks these notations can be parsed or not..

  (is.equal.wcs 0
    { int x = 0 \; return x \; })
  (is.equal.wcs 0
    { int * x = 0 \; return x \; })
  (is.equal.wcs 0
    { int * * x = 0 \; return x \; })

  (is.equal.wcs nil
    { int func \( x \) \; })
  (is.equal.wcs 0
    { int x [ 5 ] \; return x [ 0 ] \; })
  (signals.macroexpand.wcs ()
    { int x [ ] \; })

  ;; function declaration is treated as 'extern', so vanished..
  (is.equal.wcs nil
    { int x \( int \) \; })
  (is.equal.wcs nil
    { int x \( int \, float \) \; })
  (is.equal.wcs nil
    { int x \( hoge \) \; })
  (is.equal.wcs nil
    { int x \( hoge \, fuga \) \; })
  (is.equal.wcs nil
    { int x \( hoge \, fuga \, piyo \) \; })
  (is.equal.wcs nil
    { int x \( \) \; })
  (is.equal.wcs nil
    { int x \( int hoge \, short fuga \, void piyo \) \; })
  (is.equal.wcs nil
    { int x \( int hoge \, short \, void * \) \; })
  (signals.macroexpand.wcs ()
    (is.equal.wcs nil
      { int x \( hoge \, int \, int piyo \) \; }))
  (signals.macroexpand.wcs ()
    (is.equal.wcs nil
      { int x \( int \, fuga \, int piyo \) \; })))

(test test-initializer-simple
  ;; uses init-declarator
  (is.equal.wcs 0
    { int x = 0 \; return x \; })
  (is.equal.wcs 100
    {
    int x = 50 \;
    int y = x \;
    int z = y + x \;
    return z \;
    })
  ;; (signals.macroexpand.wcs ()
  ;;   { int x = { 0 \, 1 } \; })
  (is.equal.wcs t
    {
    int x [ 2 ] = { 0 \, 1 } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 \;
    })
  (is.equal.wcs t
    {
    int x [ 3 ] = { 0 \, 1 \, 2 } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 && x [ 2 ] == 2 \;
    })
  (is.equal.wcs t
    {
    int x [ 2 ] = { 0 \, 1 \, } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 \;
    })

  (is.equal.wcs 1
    {
    int x [ ] = { 0 \, 1 } \;
    return x [ 1 ] \;
    })
  ;; (signals.macroexpand.wcs ()
  ;;   { int x [ ] [ ] = { 0 \, 1 } \; })
  (is.equal.wcs 3
    {
    int x [ ] [ ] = { { 0 \, 1 } \, { 2 \, 3 } } \;
    return x [ 1 ] [ 1 ] \;
    })
  (is.equal.wcs 3
    {
    int x [ 3 ] [ 3 ] = { { 0 \, 1 } \, { 2 \, 3 } } \;
    return x [ 1 ] [ 1 ] \;
    })
  (is.equal.wcs 3
    {
    int x [ ] [ ] [ ] = { { { 0 \, 1 } \, { 2 \, 3 } } } \;
    return x [ 0 ] [ 1 ] [ 1 ] \;
    })
  ;; TODO: add multi-dimensional
  )

(test test-array-pointer-decl
  ;; NOTE: These tests only checks these notations can be parsed or not..
  (is.equal.wcs t
    {
    int * array_of_pointer [ 5 ] \;
    return (arrayp array_of_pointer) \;
    })
  (is.equal.wcs t
    {
    int \( * pointer_to_array \) [ 5 ] \;
    return (typep pointer_to_array 'pseudo-pointer) \;
    })
  (is.equal.wcs t
    {
    int * \( array_of_pointer [ 5 ] \) \;
    return (arrayp array_of_pointer) \;
    })
  (is.equal.wcs t
    {
    int \( * array_of_func_ptr [ 5 ] \) \( int \, int \) \;
    return (arrayp array_of_func_ptr) \;
    })
  (signals.macroexpand.wcs ()
    static int * array_of_func [ 5 ] \( int \, int \) \; )
  (signals.macroexpand.wcs ()
    static int * func_returns_array \( int \, int \) [ 5 ] \; )
  (signals.macroexpand.wcs ()
    static int * func_returns_func \( int x \, int y \) \( int z \) \;)
  (is.equal.wcs nil
    static int * func_returns_pointer \( int \, int \) \; )
  (is.equal.wcs t
    {
    int * \( * funcptr \) \( int \, int \) \;
    return (typep funcptr 'pseudo-pointer) \;
    })
  ;; http://unixwiz.net/techtips/reading-cdecl.html
  ;; (test '(char * \( * \( * * foo [ ] [ 8 ] \) \( \) \) [ ] \;))
  )


(test test-initializer-struct
  (is.equal.wcs t
    {
    struct hoge { int x \, y \; } foo = { 1 \, 2 } \;
    return foo \. x == 1 && foo \. y == 2 \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \, y \, z \; } foo = { 1 \, 2 \, 3 } \;
    return foo \. x == 1 && foo \. y == 2 && foo \. z == 3 \;
    })
  (is.equal.wcs t
    {
    struct { int x \, y \, z \; } foo =  { 1 \, 2 \, 3 } \;
    return foo \. x == 1 && foo \. y == 2 && foo \. z == 3 \;
    })
  (is.equal.wcs t
    {
    struct hoge { int x \, y \; } foo = { 1 \, 2 } \;
    struct fuga { int x \, y \; } bar = { 1 \, 2 } \;
    return foo \. x == bar \. x && foo \. y == bar \. y \;
    })
  (is.equal.wcs 1
    {
    struct hoge { int x \, y \; }  \;
    struct fuga { struct hoge h \; } \;
    struct fuga h = { { 1 } } \;
    return h \. h \. x \;
    })
  (is.equal.wcs 0
    {
    struct hoge { int x \, y \; } \;
    struct hoge arr [ 5 ] \;
    return arr [ 4 ] \. x \;
    })
  (is.equal.wcs 2
    {
    struct hoge { int x \, y \; } \;
    struct hoge arr [ 5 ] = { { 1 } \, { 2 } } \;
    return arr [ 1 ] \. x \;
    })

  (is.equal.wcs 2
    {
    struct hoge { int x [ 5 ] \; } \;
    struct hoge xx = { { 0 \, 1 \, 2 \, 3 \, 4 } } \;
    return xx \. x [ 2 ] \;
    }))

(test test-typedefs
  (is.equal.wcs 1
    {
    typedef int int_t \;
    void \;                             ; typedef guard
    int_t x = 1 \;
    return x \;
    })
  (is.equal.wcs 1
    {
    struct hoge { int x \, y \; } \;
    typedef struct hoge hoge_t \;
    void \;                             ; typedef guard
    hoge_t x = { 0 \, 1 } \;
    return x \. y \;
    })
  (is.equal.wcs 1
    {
    struct hoge { int x \, y \; } \;
    typedef struct hoge * hoge_p \;
    void \;                             ; typedef guard
    struct hoge x = { 0 \, 1 } \;
    hoge_p px = & x \;
    return px -> y \;
    })

  #+ignore
  (is.equal.wcs 1
    {
    typedef int int_t \;
    void \;                             ; typedef guard
    int_t int_t = 1 \;
    return int_t \;
    })
  t)
;; removal of 'typedef guard' is tested at test-preprocessor

(test test-lisptype-decls
  (is.equal.wcs '(1 2 3)
    {
    __lisp_type list x = (list 1 2 3) \;
    return x \;
    })
  (is.equalp.wcs #(1 2 3)
    {
    __lisp_type (simple-array t (3)) x
    = (make-array '(3) :initial-contents '(1 2 3)) \;
    return x \;
    }))

;; TODO: add initializer tests
