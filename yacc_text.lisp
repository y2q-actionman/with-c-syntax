(in-package :cl-user)

(asdf:load-system :yacc)
(use-package :yacc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +operators+
  '(|,|
    = *= /= %= += -= <<= >>= &= ^= \|=
    ? |:|
    \|\|
    &&
    \|
    ^
    &
    == !=
    < > <= >=
    >> <<
    + -
    * / %
    \( \)
    ++ -- sizeof
    & * + - ~ !
    [ ] \. ->
    ))
)

(defvar *enum-symbols* nil)

(defun list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
	(cond ((null value)
	       (values nil nil))
	      ((symbolp value)
	       (let ((op (member value +operators+
				 :test #'string=
				 :key #'symbol-name))
		     (en (member value *enum-symbols*)))
		 (cond (op
			;; returns the symbol of our package.
			(values (car op) value))
		       (en
			(values 'enumeration-const value))
		       (t
			(values 'id value)))))
	      ((integerp value)
	       (values 'int-const value))
	      ((characterp value)
	       (values 'char-const value))
	      ((floatp value)
	       (values 'float-const value))
	      ((stringp value)
	       (values 'string value))
	      ((listp value)
	       (values 'lisp-expression value))
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

  (defun pick-2nd (_1 x _3)
    (declare (ignore _1 _3))
    x)
  )

(define-parser *expression-parser*
  (:start-symbol exp)

  ;; http://www.swansontec.com/sopc.html
  ;; (:precedence ((:left * /) (:left + -)))

  ;; http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf
  (:terminals
   #.(append +operators+
	     '(enumeration-const id
	       int-const char-const float-const
	       string)
	     '(lisp-expression)))

  ;; TODO
  (exp
   assignment-exp
   (exp |,| assignment-exp))

  ;; TODO
  (assignment-exp
   conditional-exp
   (unary-exp assignment-operator assignment-exp))

  ;; TODO
  (assignment-operator
   = *= /= %= += -= <<= >>= &= ^= \|=)

  ;; TODO
  (conditional-exp
   logical-or-exp
   (logical-or-exp ? exp |:| conditional-exp))

  ;; TODO
  (const-exp
   conditional-exp)

  ;; TODO
  (logical-or-exp
   logical-and-exp
   (logical-or-exp \|\| logical-and-exp))

  ;; TODO
  (logical-and-exp
   inclusive-or-exp
   (logical-and-exp && inclusive-or-exp))

  ;; TODO
  (inclusive-or-exp
   exclusive-or-exp
   (inclusive-or-exp \| exclusive-or-exp))

  ;; TODO
  (exclusive-or-exp
   and-exp
   (exclusive-or-exp ^ and-exp))

  ;; TODO
  (and-exp
   equality-exp
   (and-exp & equality-exp))

  ;; TODO
  (equality-exp
   relational-exp
   (equality-exp == relational-exp)
   (equality-exp != relational-exp))

  ;; TODO
  (relational-exp
   shift-expression
   (relational-exp < shift-expression)
   (relational-exp > shift-expression)
   (relational-exp <= shift-expression)
   (relational-exp >= shift-expression))

  ;; TODO
  (shift-expression
   additive-exp
   (shift-expression << additive-exp)
   (shift-expression >> additive-exp))

  ;; TODO
  (additive-exp
   mult-exp
   (additive-exp + mult-exp)
   (additive-exp - mult-exp))

  ;; TODO
  (mult-exp
   cast-exp
   (mult-exp * cast-exp)
   (mult-exp / cast-exp)
   (mult-exp % cast-exp))

  ;; TODO
  (cast-exp
   unary-exp
   (\( type-name \) cast-exp))

  ;; TODO
  (unary-exp
   postfix-exp
   (++ unary-exp)
   (-- unary-exp)
   (unary-operator cast-exp)
   ;; (sizeof unary-exp)
   ;; (sizeof \( type-name \))
   )

  ;; TODO
  (unary-operator
   & * + - ~ !)

  (postfix-exp
   primary-exp
   (postfix-exp [ exp ]			; TODO: compound with multi-dimention
		#'(lambda (exp op1 idx op2)
		    (declare (ignore op1 op2))
		    `(aref ,exp ,idx)))
   (postfix-exp \( argument-exp-list \)) ; TODO
   (postfix-exp \( \))			 ; TODO
   (postfix-exp \. id)			 ; TODO
   (postfix-exp -> id)			 ; TODO
   (postfix-exp ++
		#'(lambda (exp op)
		    (declare (ignore op))
		    `(prog1 ,exp (incf ,exp))))
   (postfix-exp --
		#'(lambda (exp op)
		    (declare (ignore op))
		    `(prog1 ,exp (decf ,exp)))))

  (primary-exp
   id
   const
   string
   (\( exp \) #'pick-2nd)
   lisp-expression			; added
   )

  ;; TODO
  (argument-exp-list
   assignment-exp
   (argument-exp-list \, assignment-exp))

  (const
   int-const
   char-const
   float-const
   enumeration-const)			; TODO
)

;; (parse-with-lexer (list-lexer '(x * - - 2 + 3 * y)) *expression-parser*)
;; => (+ (* X (- (- 2))) (* 3 Y))	       

(defun c-expression-tranform (form)
  (parse-with-lexer (list-lexer form)
		    *expression-parser*))


(defmacro with-c-syntax (() &body body)
  (c-expression-tranform body))

#|
(with-c-syntax ()
  1 + 2)
3
|#
