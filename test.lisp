(in-package :cl-user)

(defun test (form)
  (format t "~&~S~%   ~A~%" form 
	  (c-expression-tranform form)))

(defun test-const ()
  (test '(1))
  (test '(#\a))
  (test '(1.0))
  ;; TODO: enum
  t)

(defun test-primary-exp ()
  (test '(hoge))
  (test-const)
  (test '("abc"))
  (test '( \( 111 \) ))
  (test '((+ 1 2)))
  t)

(defun test-postfix-exp ()
  (test-primary-exp)
  (test '(array [ 80 ]))
  (test '(hoge \( 1 \, 2 \, 3 \)))
  (test '(hoge \( \)))
  ;; dot
  ;; arrow
  (test '(hoge ++))
  (test '(fuga --))
  t)

(defun test-unary-exp ()
  (test-postfix-exp)
  (test '(++ hoge))
  (test '(-- hoge))
  ;; address-of
  ;; dereference
  (test '(+ hoge))
  (test '(- hoge))
  (test '(! hoge))
  ;; sizeof
  ;; sizeof
  t)
