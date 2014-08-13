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
  ;; arr
  ;; args
  ;; no arg
  ;; dot
  ;; arrow
  (test '(hoge ++))
  (test '(fuga --))
  t)
