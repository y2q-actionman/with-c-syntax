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
  (test '({ auto auto auto int \; }))
  (test '({ auto register int \; }))

  (test '({ auto \; }))
  (test '({ register \; }))
  (test '({ static \; }))
  (test '({ extern \; }))
  (test '({ typedef \; }))
  (test '({ auto auto \; }))
  (test '({ auto register \; }))

  (test '({ void \; }))
  (test '({ int \; }))
  (test '({ char \; }))
  (test '({ unsigned int \; }))
  (test '({ long long int \; }))

  (test '({ const int \; }))
  (test '({ volatile char \; }))

  (test '({ const \; }))

  (test '({ const auto unsigned int \; }))
  (test '({ const auto unsigned int float \; }))
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
  (test '({ struct hoge { int x \; int y \; int z \; } \; }))
  (test '({ struct { int x \; } \; }))
  (test '({ struct { int x \; int y \; } \; }))
  (test '({ struct a \; }))
  (test '({ union a \; }))
  t)

(defun test-init-declarator-list ()
  (test '({ int a \; }))
  (test '({ int a \, b \; }))
  (test '({ int a \, b \, c \; }))
  (test '({ int a = 0 \; }))
  (test '({ int a \, b = 1 \; }))
  (test '({ int a \, b = 1 \, c = 2 \; }))
  t)

(defun test-spec-qualifier-list ()
  (test '({ struct hoge { int x \; } \; }))
  (test '({ struct hoge { const x \; } \; }))
  (test '({ struct hoge { unsigned int x \; } \; }))
  (test '({ struct hoge { const unsigned int x \; } \; }))
  (test '({ struct hoge { const unsigned volatile int x \; } \; }))
  t)

(defun test-struct-declarator ()
  (test '({ struct hoge { int x \; } \; }))
  (test '({ struct hoge { int x \, y \; } \; }))
  (test '({ struct hoge { int x \, y \, z \; } \; }))
  (test '({ struct hoge { int x \: 3 \; } \; }))
  (test '({ struct hoge { int x \, y \: 3 \; } \; }))
  (test '({ struct hoge { int x \: 1 \, y \: 5 \, z \: 99 \; } \; }))
  t)

(defun test-enum-spec ()
  (test '({ enum hoge \; }))
  (test '({ enum hoge { x \, y = 1 \, z } \; }))
  (test '({ enum { x = 0 \, y \, z = 3 } \; } ))
  t)


(defun test-param-type-list ()
  ;; uses cast

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

  (test '({ \( int [ 5 ] \) x \; }))

  (test '({ \( int [ ] \) x \; }))
  (test '({ \( int [ ] [ ] [ ] \) x \; }))

  (test '({ \( int [ ] \( int \) \) x \; }))
  (test '({ \( int [ 1 ] \( int \) \) x \; }))

  (test '({ \( int \( int \) \) x \; })) ; using param-type-list
  (test '({ \( int \( int \, |...| \) \) x \; }))
  (test '({ \( int \( int \, int \) \) x \; }))
  (test '({ \( int \( int \, int \, const int \) \) x \; }))

  (test '({ \( int [ ] \( \) \) x \; }))
  (test '({ \( int \( \) \( \) \) x \; }))

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
  (test '({ int x [ ] \; }))
  (test '({ int x \( int \) \; }))
  (test '({ int x \( int \, float \) \; }))
  (test '({ int x \( hoge \) \; }))
  (test '({ int x \( hoge \, fuga \) \; }))
  (test '({ int x \( hoge \, fuga \, piyo \) \; }))
  (test '({ int x \( \) \; }))

  t)

(defun test-initializer ()
  ;; uses init-declarator

  (test '({ int x = 0 \; }))
  (test '({ int x = { 0 } \; }))
  (test '({ int x = { 0 \, 1 } \; }))
  (test '({ int x = { 0 \, 1 \, 2 } \; }))
  (test '({ int x = { 0 \, 1 \, } \; }))

  t)

(defun test-array-pointer-decl ()
  (test '(int * array_of_pointer [ 5 ] \; ))
  (test '(int \( * pointer_to_array \) [ 5 ] \; ))
  (test '(int * \( array_of_pointer [ 5 ] \) \; ))
  (test '(int \( * array_of_func_ptr [ 5 ] \) \( int \, int \) \; ))
  (test '(int * array_of_func [ 5 ] \( int \, int \) \; )) ; error
  (test '(int * func_returns_array \( int \, int \) [ 5 ] \; )) ; error
  (test '(int * func_returns_func \( int x \, int y \) \( int z \) \; )) ; error
  (test '(int * func_returns_pointer \( int \, int \) \; ))
  (test '(int * \( * funcptr \) \( int \, int \) \; ))
  ;; http://unixwiz.net/techtips/reading-cdecl.html
  (test '(char * \( * \( * * foo [ ] [ 8 ] \) \( \) \) [ ] \;))
  t)

;; TODO: add initializer tests

(defun test-decl ()
  (test-decl-simple)
  (test-decl-list)
  t)
