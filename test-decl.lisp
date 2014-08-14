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

