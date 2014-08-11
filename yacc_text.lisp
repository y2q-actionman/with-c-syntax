(in-package :cl-user)

(asdf:load-system :yacc)
(use-package :yacc)

;; (defconstant +operators+
;;   '(+ - * / % ! ++ -- & \| ^ && \|\| = < > \, [ ]))

(defun list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
	(cond ((null value)
	       (values nil nil))
	      ((symbolp value)
	       (let ((term (member value '(+ - * / |(| |)|)
				   :test #'string= :key #'symbol-name)))
		 (values (if term (car term) 'id) ; returns a symbol of our package.
			 value)))
	      ((integerp value)
	       (values 'int value))
	      (t
	       (error "Unexpected value ~S" value))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))
  
  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b)
  )

(define-parser *expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  
  (expression
   (expression + expression #'i2p)
   (expression - expression #'i2p)
   (expression * expression #'i2p)
   (expression / expression #'i2p)
   term)
  
  (term
   id
   int
   (- term)
   (|(| expression |)| #'k-2-3)))

;; (parse-with-lexer (list-lexer '(x * - - 2 + 3 * y)) *expression-parser*)
;; => (+ (* X (- (- 2))) (* 3 Y))	       

(defun c-expression-tranform (form)
  (parse-with-lexer (list-lexer form)
		    *expression-parser*))


(defmacro with-c-syntax (&body body)
  `(progn
     ,@(mapcar #'c-expression-tranform
	  body)))

