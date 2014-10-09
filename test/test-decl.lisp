(in-package #:with-c-syntax)

;;; declarations

(defun test-decl-simple ()
  (eval-equal nil ()
    int *xxx* \;)
  (assert (boundp '*xxx*))
  (eval-equal 1 ()
    { int a \; a = 1 \; return a \; })
  (eval-equal nil ()
    { int a \; \( void \) a \; })
  (eval-equal nil ()
    { int \; })             ; should be warned?
  (eval-equal nil ()
    { int \; int a \; int \; int b \; \( void \) a \, b \; })
  t)

(defun test-decl-list ()
  (eval-equal 1 ()
    { int a = 1 \; return a \; })
  (eval-equal 2 ()
    { int a = 1 \; int b = 2 \; return a \, b \; })
  (eval-equal 3 ()
    { int a = 1 \; int b = 2 \; int c = 3 \; return a \, b \, c \; })
  t)

;; TODO: add 'global' decl -- into translation-unit tests.
;; TODO: add 'static' decl's extent test.
;; TODO: add typedef
(defun test-decl-specs ()
  ;; storage-class
  (eval-equal 1 ()
    { int x = 1 \; return x \; })
  (eval-equal 2 ()
    { auto int x = 2 \; return x \; })
  (eval-equal 3 ()
    { register int x = 3 \; return x \; })
  (eval-equal 4 ()
    { static int x = 4 \; return x \; })
  (let ((x 5))
    (eval-equal 5 ()
      { extern int x \; return x \; }))
  (assert-compile-error ()
    { extern int x = 999 \; })
  (eval-equal nil ()
    { typedef int \; })
  (assert-compile-error ()
    { typedef int x = 999 \; })
  (assert-compile-error ()
    { auto auto auto int \; })
  (assert-compile-error ()
    { auto register int \; })

  (eval-equal 6 ()
    { auto x = 6 \; return x \; })
  (eval-equal 7 ()
    { register x = 7 \; return x \; })
  (eval-equal 8 ()
    { static x = 8 \; return x \; })
  (let ((x 9))
    (eval-equal 9 ()
      { extern x \; return x \; }))
  (assert-compile-error ()
    { extern x = 999 \; })
  (eval-equal nil ()
    { typedef x  \; })
  (assert-compile-error ()
    { typedef x = 999 \; })
  (assert-compile-error ()
    { auto auto \; })
  (assert-compile-error ()
    { auto register \; })

  (eval-equal nil ()
    int func \( \) \;)
  (assert-compile-error ()
    auto int func \( \) \;)
  (assert-compile-error ()
    register int func \( \) \;)
  (eval-equal nil ()
    static int func \( \) \;)
  (eval-equal nil ()
    extern int func \( \) \;)
  (assert-compile-error ()
    typedef int func \( \) \;)

  ;; type-spec
  (assert-compile-error ()
    { void x \; })

  (eval-equal 10 ()
    { int x = 10 \; return x \; })
  (eval-equal 11 ()
    { signed int x = 11 \; return x \; })
  (eval-equal 11 ()
    { unsigned int x = 11 \; return x \; })
  (assert-compile-error ()
    { signed signed x \; })
  (assert-compile-error ()
    { signed unsigned x \; })
  (assert-compile-error ()
    { unsigned unsigned x \; })
  (eval-equal 12 ()
    { short int x = 12 \; return x \; })
  (eval-equal 13 ()
    { short signed int x = 13 \; return x \; })
  (eval-equal 14 ()
    { short unsigned int x = 14 \; return x \; })
  (assert-compile-error ()
    { short short x \; })
  (eval-equal 15 ()
    { long int x = 15 \; return x \; })
  (eval-equal 16 ()
    { signed long int x = 16 \; return x \; })
  (eval-equal 17 ()
    { unsigned long int x = 17 \; return x \; })
  (eval-equal 18 ()
    { long long int x = 18 \; return x \; })
  (eval-equal 19 ()
    { signed long long int x = 19 \; return x \; })
  (eval-equal 20 ()
    { unsigned long long int x = 20 \; return x \; })
  (assert-compile-error ()
    { long long long \; })

  (eval-equal 21 ()
    { char x = 21 \; return x \; })
  (eval-equal 22 ()
    { signed char x = 22 \; return x \; })
  (eval-equal 23 ()
    { char unsigned x = 23 \; return x \; })
  (assert-compile-error ()
    { short char \; })
  (assert-compile-error ()
    { long char \; })

  (eval-equal 1f1 ()
    { float x = 1f1 \; return x \; })
  (eval-equal 2s2 ()			; This is our extension.
    { short float x = 2s2 \; return x \; })
  (assert-compile-error ()
    { long float \; })
  (assert-compile-error ()
    { signed float \; })
  (assert-compile-error ()
    { unsigned float \; })

  (eval-equal 3d3 ()
    { double x = 3d3 \; return x \; })
  (eval-equal 4l4 ()
    { long double x = 4l4 \; return x \; })
  (assert-compile-error ()
    { short double \; })
  (assert-compile-error ()
    { signed double \; })
  (assert-compile-error ()
    { unsigned double \; })

  ;; cv-qualifier
  (eval-equal 30 ()
    { const int x = 30 \; return x \; })
  (eval-equal 31 ()
    { int const x = 31 \; return x \; })
  (eval-equal 32 ()
    { const x = 32 \; return x \; })
  ;; TODO: support this?
  ;; (assert-compile-error ()
  ;;   { int const x = 0 \; x = 1 \; })

  (eval-equal 33 ()
    { volatile int x = 33 \; return x \; })
  (eval-equal 34 ()
    { int volatile x = 34 \; return x \; })
  (eval-equal 35 ()
    { volatile x = 35 \; return x \; })

  (eval-equal 36 ()
    { const volatile register signed short int x = 36 \; return x \; })

  (assert-compile-error ()
    { const auto unsigned int float \; })
  t)

(defun test-struct-or-union-spec ()
  (assert-compile-error ()
    { struct \; })
  (assert-compile-error ()
    { union \; })
  (eval-equal 100 ()
    {
    struct hoge { int x \; } foo \;
    foo \. x = 100 \;
    return foo \. x \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \; int y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \; int y \; int z \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (eval-equal 1 ()
    {
    struct { int x \; } foo \;
    foo \. x = 1 \;
    return foo \. x \;
    })
  (eval-equal t ()
    {
    struct { int x \; int y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (eval-equal t ()
    {
    struct a \;
    struct a * p \;
    \( void \) p \;
    return t \;
    })
  (eval-equal t ()
    {
    union a \;
    union a * p \;
    \( void \) p \;
    return t \;
    })
  (eval-equal 100 ()
    {
    union hoge { int x \; int y \; } foo \;
    foo \. x = 100 \;
    return foo \. x \;
    })
  t)

(defun test-init-declarator-list ()
  (eval-equal t ()
    { int a \; \( void \) a \; return t \; })
  (eval-equal t ()
    { int a \, b \; \( void \) a \, b \; return t \; })
  (eval-equal t ()
    { int a \, b \, c \; \( void \) a \, b \, c \; return t \; })
  (eval-equal 0 ()
    { int a = 0 \; return a \; })
  (eval-equal 1 ()
    { int a \, b = 1 \; \( void \) a \; return b \; })
  (eval-equal t ()
    { int a \, b = 1 \, c = 2 \; \( void \) a \; return b == 1 && c == 2 \; })
  t)

;; TODO: add cv-qualifier tests!
(defun test-spec-qualifier-list ()
  (eval-equal 100 ()
    {
    struct hoge { int x \; } foo = { 100 } \;
    return foo \. x \;
    })
  (eval-equal 101 ()
    {
    struct hoge { unsigned x \; } foo = { 101 } \;
    return foo \. x \;
    })

  (eval-equal 100 ()
    {
    struct hoge { const x \; } foo = { 100 } \;
    return foo \. x \;
    })
  ;; TODO: suport const
  #+ignore
  (assert-runtime-error ()
    {
    struct hoge { const x \; } foo \;
    foo \. x = 99 \;
    })

  (eval-equal 200 ()
    {
    struct hoge { volatile x \; } foo \;
    foo \. x = 200 \;
    return foo \. x \;
    })

  (eval-equal 300 ()
    {
    struct hoge { const unsigned volatile int x \; } foo = { 300 } \;
    return foo \. x \;
    })
  ;; TODO: suport const
  #+ignore
  (assert-runtime-error ()
    {
    struct hoge { const unsigned volatile int x \; } foo = { 300 } \;
    foo \. x = 9999 \;
    })
  t)

(defun test-struct-declarator ()
  (eval-equal t ()
    { struct hoge { int x \; } foo \; \( void \) foo \; return t \; })
  (eval-equal t ()
    {
    struct hoge { int x \, y \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \, y \, z \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \: 3 \; } foo \;
    foo \. x = 1 \;
    return foo \. x == 1 \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \, y \: 3 \; } foo \;
    foo \. x = foo \. y = 1 \;
    return foo \. x == foo \. y \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \: 1 \, y \: 5 \, z \: 8 \; } foo \;
    foo \. x = foo \. y = foo \. z = 1 \;
    return foo \. x == foo \. y && foo \. y == foo \. z \;
    })
  (assert-compile-error ()
    { struct hoge { int x \: 99 \; } \; })
  t)

(defun test-enum-spec ()
  (eval-equal 100 ()
    {
    enum hoge \;
    enum hoge foo = 100 \;
    return foo \;
    })
  (eval-equal t ()
    {
    enum hoge { x \, y = 4 \, z } \;
    return x == 0 && y == 4 && z == 5 \;
    })
  (eval-equal t ()
    {
    enum { x = 0 \, y \, z = 3 } \;
    return x == 0 && y == 1 && z == 3 \;
    })
  t)


(defun test-param-type-list ()
  ;; NOTE: These 'sizeof' test only checks these notations can be parsed or not..
  (eval-equal 1 ()
    { return sizeof \( int \( int \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( unsigned int \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( int * \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( int * * \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( const unsigned int * hoge \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( const struct hogehoge * * \) \) \; })
  t)

(defun test-type-name ()
  ;; uses cast
  (eval-equal 5 ()
    { unsigned x = 5 \; return \( int \) x \; })
  (eval-equal 8 ()
    { int x = 8 \; return \( unsigned int \) x \; })

  ;; NOTE: These 'sizeof' test only checks these notations can be parsed or not..

  ;; abstract-declarator -- pointer
  (eval-equal 1 ()
    { return sizeof \( int * \) \; })
  (eval-equal 1 ()
    { return sizeof \( int const * \) \; }) 	; not included in pointer..
  (eval-equal 1 ()
    { return sizeof \( int * const \) \; })
  (eval-equal 1 ()
    { return sizeof \( int * * \) \; })
  (eval-equal 1 ()
    { return sizeof \( int * const * const * \) \; })

  ;; abstract-declarator -- direct-abstract-declarator
  (eval-equal 1 ()
    { return sizeof \( int \( * \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( * \( int \) \) \) \; })

  (eval-equal 2 ()
    { return sizeof \( int [ 1 ] [ 2 ] \) \; })
  (eval-equal 6 ()
    { return sizeof \( int [ 1 ] [ 2 ] [ 3 ] \) \; })
  (eval-equal 5 ()
    { return sizeof \( int [ 5 ] \) \; })

  (assert-compile-error ()		; empty dims
    { return sizeof \( int [ ] \) \; })
  (assert-compile-error ()		; empty dims
    { return sizeof \( int [ ] [ ] [ ] \) \; })

  (assert-compile-error ()		; array of funcs
    { return sizeof \( int [ ] \( int \) \) \; })

  ;; 'sizeof' to function. (It is a weird extension..)
  (eval-equal 1 ()			; using param-type-list
    { return sizeof \( int \( int \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( int \, |...| \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( int \, int \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int \( int \, int \, const int \) \) \; })

  (assert-compile-error ()		; array of funcs
    { return sizeof \( int [ ] \( \) \) \; })
  (assert-compile-error ()		; func returns a func
    { return sizeof \( int \( \) \( \) \) \; })
  (eval-equal 1 ()			; K&R-style func
    { return sizeof \( int \( \) \) \; })

  ;; abstract-declarator
  (eval-equal 1 ()
    { return sizeof \( int * \( * \) \) \; })
  (eval-equal 1 ()
    { return sizeof \( int * \( * \( int \) \) \) \; })
  t)


(defun test-declarator ()
  ;; uses init-declarator

  ;; NOTE: These tests only checks these notations can be parsed or not..

  (eval-equal 0 ()
    { int x = 0 \; return x \; })
  (eval-equal 0 ()
    { int * x = 0 \; return x \; })
  (eval-equal 0 ()
    { int * * x = 0 \; return x \; })

  (eval-equal nil ()
    { int func \( x \) \; })
  (eval-equal 0 ()
    { int x [ 5 ] \; return x [ 0 ] \; })
  (assert-compile-error ()
    { int x [ ] \; })

  ;; function declaration is treated as 'extern', so vanished..
  (eval-equal nil ()
    { int x \( int \) \; })
  (eval-equal nil ()
    { int x \( int \, float \) \; })
  (eval-equal nil ()
    { int x \( hoge \) \; })
  (eval-equal nil ()
    { int x \( hoge \, fuga \) \; })
  (eval-equal nil ()
    { int x \( hoge \, fuga \, piyo \) \; })
  (eval-equal nil ()
    { int x \( \) \; })

  t)

(defun test-initializer-simple ()
  ;; uses init-declarator
  (eval-equal 0 ()
    { int x = 0 \; return x \; })
  (eval-equal 100 ()
    {
    int x = 50 \;
    int y = x \;
    int z = y + x \;
    return z \;
    })
  ;; (assert-compile-error ()
  ;;   { int x = { 0 \, 1 } \; })
  (eval-equal t ()
    {
    int x [ 2 ] = { 0 \, 1 } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 \;
    })
  (eval-equal t ()
    {
    int x [ 3 ] = { 0 \, 1 \, 2 } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 && x [ 2 ] == 2 \;
    })
  (eval-equal t ()
    {
    int x [ 2 ] = { 0 \, 1 \, } \;
    return x [ 0 ] == 0 && x [ 1 ] == 1 \;
    })

  (eval-equal 1 ()
    {
    int x [ ] = { 0 \, 1 } \;
    return x [ 1 ] \;
    })
  ;; (assert-compile-error ()
  ;;   { int x [ ] [ ] = { 0 \, 1 } \; })
  (eval-equal 3 ()
    {
    int x [ ] [ ] = { { 0 \, 1 } \, { 2 \, 3 } } \;
    return x [ 1 ] [ 1 ] \;
    })
  (eval-equal 3 ()
    {
    int x [ 3 ] [ 3 ] = { { 0 \, 1 } \, { 2 \, 3 } } \;
    return x [ 1 ] [ 1 ] \;
    })
  (eval-equal 3 ()
    {
    int x [ ] [ ] [ ] = { { { 0 \, 1 } \, { 2 \, 3 } } } \;
    return x [ 0 ] [ 1 ] [ 1 ] \;
    })
  ;; TODO: add multi-dimensionals
  t)

(defun test-array-pointer-decl ()
  ;; NOTE: These tests only checks these notations can be parsed or not..
  (eval-equal t ()
    {
    int * array_of_pointer [ 5 ] \;
    return (arrayp array_of_pointer) \;
    })
  (eval-equal t ()
    {
    int \( * pointer_to_array \) [ 5 ] \;
    return (typep pointer_to_array 'pseudo-pointer) \;
    })
  (eval-equal t ()
    {
    int * \( array_of_pointer [ 5 ] \) \;
    return (arrayp array_of_pointer) \;
    })
  (eval-equal t ()
    {
    int \( * array_of_func_ptr [ 5 ] \) \( int \, int \) \;
    return (arrayp array_of_func_ptr) \;
    })
  (assert-compile-error ()
    static int * array_of_func [ 5 ] \( int \, int \) \; )
  (assert-compile-error ()
    static int * func_returns_array \( int \, int \) [ 5 ] \; )
  (assert-compile-error ()
    static int * func_returns_func \( int x \, int y \) \( int z \) \;)
  (eval-equal nil ()
    static int * func_returns_pointer \( int \, int \) \; )
  (eval-equal t ()
    {
    int * \( * funcptr \) \( int \, int \) \;
    return (typep funcptr 'pseudo-pointer) \;
    })
  ;; http://unixwiz.net/techtips/reading-cdecl.html
  ;; (test '(char * \( * \( * * foo [ ] [ 8 ] \) \( \) \) [ ] \;))
  t)


(defun test-initializer-struct ()
  (eval-equal t ()
    {
    struct hoge { int x \, y \; } foo = { 1 \, 2 } \;
    return foo \. x == 1 && foo \. y == 2 \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \, y \, z \; } foo = { 1 \, 2 \, 3 } \;
    return foo \. x == 1 && foo \. y == 2 && foo \. z == 3 \;
    })
  (eval-equal t ()
    {
    struct { int x \, y \, z \; } foo =  { 1 \, 2 \, 3 } \;
    return foo \. x == 1 && foo \. y == 2 && foo \. z == 3 \;
    })
  (eval-equal t ()
    {
    struct hoge { int x \, y \; } foo = { 1 \, 2 } \;
    struct fuga { int x \, y \; } bar = { 1 \, 2 } \;
    return foo \. x == bar \. x && foo \. y == bar \. y \;
    })
  (eval-equal 1 ()
    {
    struct hoge { int x \, y \; }  \;
    struct fuga { struct hoge h \; } \;
    struct fuga h = { { 1 } } \;
    return h \. h \. x \;
    })
  (eval-equal 0 ()
    {
    struct hoge { int x \, y \; } \;
    struct hoge arr [ 5 ] \;
    return arr [ 4 ] \. x \;
    })
  (eval-equal 2 ()
    {
    struct hoge { int x \, y \; } \;
    struct hoge arr [ 5 ] = { { 1 } \, { 2 } } \;
    return arr [ 1 ] \. x \;
    })
  t)

;; TODO: remove the KLUDGE 'typedef guard'
(defun test-typedefs ()
  (eval-equal 1 ()
    {
    typedef int int_t \;
    void \;                             ; typedef guard
    int_t x = 1 \;
    return x \;
    })

  (eval-equal 1 ()
    {
    struct hoge { int x \, y \; } \;
    typedef struct hoge hoge_t \;
    void \;                             ; typedef guard
    hoge_t x = { 0 \, 1 } \;
    return x \. y \;
    })

  (eval-equal 1 ()
    {
    struct hoge { int x \, y \; } \;
    typedef struct hoge * hoge_p \;
    void \;                             ; typedef guard
    struct hoge x = { 0 \, 1 } \;
    hoge_p px = & x \;
    return px -> y \;
    })

  #+ignore
  (eval-equal 1 ()
    {
    typedef int int_t \;
    void \;                             ; typedef guard
    int_t int_t = 1 \;
    return int_t \;
    })
  t)

(defun test-lisptype-decls ()
  (eval-equal '(1 2 3) ()
    {
    (type list) x = (list 1 2 3) \;
    return x \;
    })
  t)

;; TODO: add initializer tests

(defun test-decl ()
  (test-decl-simple)
  (test-decl-list)
  (test-decl-specs)
  (test-struct-or-union-spec)
  (test-init-declarator-list)
  (test-spec-qualifier-list)
  (test-struct-declarator)
  (test-enum-spec)
  (test-param-type-list)
  (test-type-name)
  (test-declarator)
  (test-initializer-simple)
  (test-array-pointer-decl)
  (test-initializer-struct)
  (test-typedefs)
  t)
