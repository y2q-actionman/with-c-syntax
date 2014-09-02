(in-package :with-c-syntax)

;;; declarations

(defun test-decl-simple ()
  (test '({ int a \; }))
  (test '({ int a \; a = 1 \; }))
  t)

(defun test-decl ()
  (test '({ int a \; }))
  (test '({ int \; }))
  (test '({ int \; int a \; int \; int b \; }))
  t)

(defun test-decl-list ()
  (test '({ int a \; }))
  (test '({ int a \; int b \; }))
  (test '({ int a \; int b \; int c \; }))
  t)

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
  (test '({ struct hoge { int x \; } \; }))
  (test '({ struct hoge { int x \; int y \; } \; }))
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
