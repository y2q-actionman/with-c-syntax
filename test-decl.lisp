(in-package :cl-user)

(defun test (form)
  (format t "~&~S~%   ~S~%" form 
	  (c-expression-tranform form)))

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

(defun test-enum-spec ()
  (test '({ enum hoge \; }))
  (test '({ enum hoge { x \, y = 1 \, z } \; }))
  (test '({ enum { x = 0 \, y \, z = 3 } \; } ))
  t)
