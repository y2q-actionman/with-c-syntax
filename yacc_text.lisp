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

  (defconstant +keywords+
    '(\;
      case default
      { }
      if else switch
      while do for
      goto continue break return
      ))
  )

(defvar *enum-symbols* nil)

(defun list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
	(cond ((null value)
	       (values nil nil))
	      ((symbolp value)
	       (let ((op (or (member value +operators+
				     :test #'string=
				     :key #'symbol-name)
			     (member value +keywords+
				     :test #'string=
				     :key #'symbol-name)))
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
  (defun lispify-unary (op)
    #'(lambda (_ exp)
	(declare (ignore _))
	`(,op ,exp)))
  
  (defun lispify-binary (op)
    #'(lambda (exp1 _ exp2)
	(declare (ignore _))
	`(,op ,exp1 ,exp2)))

  (defun lispify-augmented-assignment (op)
    #'(lambda (exp1 _ exp2)
	(declare (ignore _))
	(let ((tmp (gensym)))
	  `(let ((,tmp ,exp1))
	     (setf ,exp1
		   (,op ,tmp ,exp2))))))

  (defun pick-2nd (_1 x _3)
    (declare (ignore _1 _3))
    x)

  (defun ash-right (i c)
    (ash i (- c)))
  )

(define-parser *expression-parser*
  (:start-symbol stat)

  ;; http://www.swansontec.com/sopc.html
  (:precedence (;; Primary expression
		(:left \( \) [ ] \. -> ++ --)
		;; Unary
		(:right * & + - ! ~ ++ -- #+ignore(typecast) sizeof)
		;; Binary
		(:left * / %)
		(:left + -)
		(:left >> <<)
		(:left < > <= >=)
		(:left == !=)
		(:left &)
		(:left ^)
		(:left \|)
		(:left &&)
		(:left \|\|)
		;; Ternary
		(:right ? \:)
		;; Assignment
		(:right = += -= *= /= %= >>= <<= &= ^= \|=)
		;; Comma
		(:left \,)
		))

  ;; http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf
  (:terminals
   #.(append +operators+
	     +keywords+
	     '(enumeration-const id
	       int-const char-const float-const
	       string)
	     '(lisp-expression)))

  ;; TODO
  (stat
   labeled_stat
   exp_stat
   compound_stat
   selection_stat
   iteration_stat
   jump_stat)

  ;; TODO
  (labeled_stat
   (id \: stat)
   (case const_exp \: stat)
   (default \: stat))

  (exp_stat
   (exp \;
	#'(lambda (exp term)
	    (declare (ignore term))
	    exp))
   (\;
    #'(lambda (term)
	(declare (ignore term))
	nil)))

  (compound_stat
   ;; ({ decl_list stat_list })
   ({ stat_list }
      #'(lambda (op1 sts op2)
	  (declare (ignore op1 op2))
	  `(progn ,@sts)))
   ;; ({ decl_list	})
   ({ }
      #'(lambda (op1 op2)
	  (declare (ignore op1 op2))
	  '(progn))))

  (stat_list
   (stat
    #'list)
   (stat_list stat
	      #'(lambda (sts st)
		  (append sts (list st)))))

  (selection_stat
   (if \( exp \) stat
       #'(lambda (op lp exp rp stat)
	   (declare (ignore op lp rp))
	   `(if ,exp ,stat)))
   (if \( exp \) stat else stat
       #'(lambda (op lp exp rp stat1 el stat2)
	   (declare (ignore op lp rp el))
	   `(if ,exp ,stat1 ,stat2)))
   (switch \( exp \) stat))		; TODO

  ;; TODO
  (iteration_stat
   (while \( exp \) stat)
   (do stat while \( exp \) \;)
   (for \( exp \; exp \; exp \) stat)
   (for \( exp \; exp \;     \) stat)
   (for \( exp \;     \; exp \) stat)
   (for \( exp \;     \;     \) stat)
   (for \(     \; exp \; exp \) stat)
   (for \(     \; exp \;     \) stat)
   (for \(     \;     \; exp \) stat)
   (for \(     \;     \;     \) stat))

  ;; TODO
  (jump_stat
   (goto id \;)
   (continue \;)
   (break \;)
   (return exp \;)
   (return \;))


  (exp
   assignment-exp
   (exp |,| assignment-exp
	(lispify-binary 'progn)))

  ;; 'assignment-operator' is included here
  (assignment-exp
   conditional-exp
   (unary-exp = assignment-exp
	      #'(lambda (exp1 op exp2)
		  (declare (ignore op))
		  `(setf ,exp1 ,exp2)))
   (unary-exp *= assignment-exp
	      (lispify-augmented-assignment '*))
   (unary-exp /= assignment-exp
	      (lispify-augmented-assignment '/))
   (unary-exp %= assignment-exp
	      (lispify-augmented-assignment 'mod))
   (unary-exp += assignment-exp
	      (lispify-augmented-assignment '+))
   (unary-exp -= assignment-exp
	      (lispify-augmented-assignment '-))
   (unary-exp <<= assignment-exp
	      (lispify-augmented-assignment 'ash))
   (unary-exp >>= assignment-exp
	      (lispify-augmented-assignment 'ash-right))
   (unary-exp &= assignment-exp
	      (lispify-augmented-assignment 'logand))
   (unary-exp ^= assignment-exp
	      (lispify-augmented-assignment 'logxor))
   (unary-exp \|= assignment-exp
	      (lispify-augmented-assignment 'logior)))

  (conditional-exp
   logical-or-exp
   (logical-or-exp ? exp |:| conditional-exp
		   #'(lambda (cnd op1 then-exp op2 else-exp)
		       (declare (ignore op1 op2))
		       `(if ,cnd ,then-exp ,else-exp))))

  ;; TODO
  (const-exp
   conditional-exp)

  (logical-or-exp
   logical-and-exp
   (logical-or-exp \|\| logical-and-exp
		    (lispify-binary 'or)))

  (logical-and-exp
   inclusive-or-exp
   (logical-and-exp && inclusive-or-exp
		    (lispify-binary 'and)))

  (inclusive-or-exp
   exclusive-or-exp
   (inclusive-or-exp \| exclusive-or-exp
		     (lispify-binary 'logior)))

  (exclusive-or-exp
   and-exp
   (exclusive-or-exp ^ and-exp
		     (lispify-binary 'logxor)))

  (and-exp
   equality-exp
   (and-exp & equality-exp
	    (lispify-binary 'logand)))

  (equality-exp
   relational-exp
   (equality-exp == relational-exp
		 (lispify-binary '=))
   (equality-exp != relational-exp
		 (lispify-binary '/=)))

  (relational-exp
   shift-expression
   (relational-exp < shift-expression
		   (lispify-binary '<))
   (relational-exp > shift-expression
		   (lispify-binary '>))
   (relational-exp <= shift-expression
		   (lispify-binary '<=))
   (relational-exp >= shift-expression
		   (lispify-binary '>=)))

  (shift-expression
   additive-exp
   (shift-expression << additive-exp
		     (lispify-binary 'ash))
   (shift-expression >> additive-exp
		     (lispify-binary 'ash-right)))

  (additive-exp
   mult-exp
   (additive-exp + mult-exp
		 (lispify-binary '+))
   (additive-exp - mult-exp
		 (lispify-binary '-)))

  (mult-exp
   cast-exp
   (mult-exp * cast-exp
	     (lispify-binary '*))
   (mult-exp / cast-exp
	     (lispify-binary '/))
   (mult-exp % cast-exp
	     (lispify-binary 'mod)))

  (cast-exp
   unary-exp
   (\( type-name \) cast-exp		; TODO: type-name must be defined
       #'(lambda (op1 type op2 exp)
	   (declare (ignore op1 op2))
	   `(coerce ,exp ',type))))

  ;; 'unary-operator' is included here
  (unary-exp
   postfix-exp
   (++ unary-exp
       (lispify-unary 'incf))
   (-- unary-exp
       (lispify-unary 'decf))
   (& cast-exp)				; TODO
   (* cast-exp)				; TODO
   (+ cast-exp
      (lispify-unary '+))
   (- cast-exp
      (lispify-unary '-))
   (! cast-exp
      (lispify-unary 'not))
   (sizeof unary-exp)			; TODO
   (sizeof \( type-name \)))		; TODO

  (postfix-exp
   primary-exp
   (postfix-exp [ exp ]			; TODO: compound with multi-dimention
		#'(lambda (exp op1 idx op2)
		    (declare (ignore op1 op2))
		    `(aref ,exp ,idx)))
   (postfix-exp \( argument-exp-list \)
		#'(lambda (exp op1 args op2)
		    (declare (ignore op1 op2))
		    `(apply ,exp ,args)))
   (postfix-exp \( \)
		#'(lambda (exp op1 op2)
		    (declare (ignore op1 op2))
		    `(funcall ,exp)))
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
   (\( exp \)
       #'pick-2nd)
   lisp-expression)			; added

  (argument-exp-list
   (assignment-exp
    #'list)
   (argument-exp-list \, assignment-exp
		      #'(lambda (exp1 op exp2)
			  (declare (ignore op))
			  (append exp1 (list exp2)))))

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
