(in-package :with-c-syntax)

;;; declarations

(defun test-decl-simple ()
  (eval-equal nil ()
    int a \;)
  (eval-equal 1 ()
    { int a \; a = 1 \; return a \; })
  (eval-equal nil ()
    { int a \; })
  (eval-equal nil ()
    { int \; })             ; should be warned?
  (eval-equal nil ()
    { int \; int a \; int \; int b \; })
  t)

(defun test-decl-list ()
  (eval-equal 1 ()
    { int a = 1 \; return a \; })
  (eval-equal 2 ()
    { int a = 1 \; int b = 2 \; return a \, b \; })
  (eval-equal 3 ()
    { int a = 1 \; int b = 2 \; int c = 3 \; return a \, b \, c \; })
  t)

;; TODO: add storage-class tests!
(defun test-decl-specs ()
  (test '({ int \; }))

  (test '({ auto int \; }))
  (test '({ register int \; }))
  (test '({ static int \; }))
  (test '({ extern int \; }))
  (test '({ typedef int \; }))
  (assert-compile-error ()
    { auto auto auto int \; })
  (assert-compile-error ()
    { auto register int \; })

  (test '({ auto \; }))
  (test '({ register \; }))
  (test '({ static \; }))
  (test '({ extern \; }))
  (test '({ typedef \; }))
  (assert-compile-error ()
    { auto auto \; })
  (assert-compile-error ()
    { auto register \; })

  (test '({ void \; }))
  (test '({ int \; }))
  (test '({ char \; }))
  (test '({ unsigned int \; }))
  (test '({ long long int \; }))

  (test '({ const int \; }))
  (test '({ volatile char \; }))

  (test '({ const \; }))

  (test '({ const auto unsigned int \; }))
  (assert-compile-error ()
    { const auto unsigned int float \; })
  t)

(defun test-struct-or-union-spec ()
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
    return t \;
    })
  ;; TODO: add union tests!
  (eval-equal t ()
    {
    union a \;
    union a * p \;
    return t \;
    })
  t)

(defun test-init-declarator-list ()
  (eval-equal t ()
    { int a \; return t \; })
  (eval-equal t ()
    { int a \, b \; return t \; })
  (eval-equal t ()
    { int a \, b \, c \; return t \; })
  (eval-equal 0 ()
    { int a = 0 \; return a \; })
  (eval-equal 1 ()
    { int a \, b = 1 \; return b \; })
  (eval-equal t ()
    { int a \, b = 1 \, c = 2 \; return b == 1 && c == 2 \; })
  t)

;; TODO: add cv-qualifier tests!
(defun test-spec-qualifier-list ()
  (test '({ struct hoge { int x \; } \; }))
  (test '({ struct hoge { const x \; } \; }))
  (test '({ struct hoge { unsigned int x \; } \; }))
  (test '({ struct hoge { const unsigned int x \; } \; }))
  (test '({ struct hoge { const unsigned volatile int x \; } \; }))
  t)

(defun test-struct-declarator ()
  (eval-equal t ()
    { struct hoge { int x \; } foo \; return t \; })
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
  ;; TODO: support cast
  (test '({ \( int \( int \) \) x \; }))
  (test '({ \( int \( unsigned int \) \) x \; }))
  (test '({ \( int \( int * \) \) x \; }))
  (test '({ \( int \( int * * \) \) x \; }))
  (test '({ \( int \( const unsigned int * hoge \) \) x \; }))
  (test '({ \( int \( const struct hogehoge * * \) \) x \; }))

  t)

(defun test-type-name ()
  ;; uses cast

  (test '({ \( int \) x \; }))
  (test '({ \( unsigned int \) x \; }))

  ;; abstract-declarator -- pointer
  (test '({ \( int * \) x \; }))
  (test '({ \( int const * \) x \; })) 	; not included in pointer..
  (test '({ \( int * const \) x \; }))
  (test '({ \( int * * \) x \; }))
  (test '({ \( int * const * const * \) x \; }))

  ;; abstract-declarator -- direct-abstract-declarator
  (test '({ \( int \( * \) \) x \; }))
  (test '({ \( int \( * \( int \) \) \) x \; }))

  (test '({ \( int [ 1 ] [ 2 ] \) x \; }))
  (test '({ \( int [ 1 ] [ 2 ] [ 3 ] \) x \; }))

  ;; (test '({ \( int [ 5 ] \) x \; }))

  ;; (test '({ \( int [ ] \) x \; }))
  ;; (test '({ \( int [ ] [ ] [ ] \) x \; }))

  ;; (test '({ \( int [ ] \( int \) \) x \; }))
  ;; (test '({ \( int [ 1 ] \( int \) \) x \; }))

  (test '({ \( int \( int \) \) x \; })) ; using param-type-list
  (test '({ \( int \( int \, |...| \) \) x \; }))
  (test '({ \( int \( int \, int \) \) x \; }))
  (test '({ \( int \( int \, int \, const int \) \) x \; }))

  ;; (test '({ \( int [ ] \( \) \) x \; }))
  ;; (test '({ \( int \( \) \( \) \) x \; }))

  (test '({ \( int \( \) \) x \; }))

  ;; abstract-declarator
  (test '({ \( int * \( * \) \) x \; }))
  (test '({ \( int * \( * \( int \) \) \) x \; }))

  t)


(defun test-declarator ()
  ;; uses init-declarator

  (test '({ int x \; }))
  (test '({ int * x \; }))
  (test '({ int * * x \; }))

  (test '({ int x \; }))

  (test '({ int \( x \) \; }))
  (test '({ int x [ 5 ] \; }))
  (assert-compile-error ()
    { int x [ ] \; })
  (test '({ int x \( int \) \; }))
  (test '({ int x \( int \, float \) \; }))
  (test '({ int x \( hoge \) \; }))
  (test '({ int x \( hoge \, fuga \) \; }))
  (test '({ int x \( hoge \, fuga \, piyo \) \; }))
  (test '({ int x \( \) \; }))

  t)

(defun test-initializer-simple ()
  ;; uses init-declarator
  (eval-equal 0 ()
    { int x = 0 \; return x \; })
  (assert-compile-error ()
    { int x = { 0 } \; })
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
  ;; TODO: support this!
  ;; (test '({ int x [ ]= { 0 \, 1 } \; }))
  t)

(defun test-array-pointer-decl ()
  (test '(int * array_of_pointer [ 5 ] \; ))
  (test '(int \( * pointer_to_array \) [ 5 ] \; ))
  (test '(int * \( array_of_pointer [ 5 ] \) \; ))
  (test '(int \( * array_of_func_ptr [ 5 ] \) \( int \, int \) \; ))
  (assert-compile-error ()
    int * array_of_func [ 5 ] \( int \, int \) \; )
  (assert-compile-error ()
    int * func_returns_array \( int \, int \) [ 5 ] \; )
  (assert-compile-error ()
    int * func_returns_func \( int x \, int y \) \( int z \) \;)
  (test '(int * func_returns_pointer \( int \, int \) \; ))
  (test '(int * \( * funcptr \) \( int \, int \) \; ))
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
  ;; TODO: add nested struct initializer
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
  t)
