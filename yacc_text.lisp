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
      auto register static extern typedef
      void char short int long float double signed unsigned
      const volatile
      struct union
      enum
      |...|
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
  (defvar *declarations* nil
    "list of (symbol &optional init-exp)")

  (defvar *break-statements* nil
    "list of (go 'break), should be rewrited")

  (defvar *continue-statements* nil
    "list of (go 'continue), should be rewrited")

  (defvar *case-label-list* nil
    "alist of (<gensym> . :exp <case-exp>)")

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

  (defun ash-right (i c)
    (ash i (- c)))


  (defun extract-if-statement (exp then-body
			       &optional (else-body nil))
    (let* ((then-tag (gensym "(if then)"))
	   (else-tag (gensym "(if else)"))
	   (end-tag (gensym "(if end)")))
      `((if ,exp (go ,then-tag) (go ,else-tag))
	,then-tag
	,@then-body
	(go ,end-tag)
	,else-tag
	,@else-body
	(go ,end-tag)
	,end-tag)))

  (defun rewrite-break-statements (sym)
    (loop for i in *break-statements*
       do (setf (second i) sym))
    (setf *break-statements* nil))

  (defun rewrite-continue-statements (sym)
    (loop for i in *continue-statements*
       do (setf (second i) sym))
    (setf *continue-statements* nil))

  (defun extract-loop (body
		       &key (init nil) (cond t) (step nil)
		       (post-test-p nil))
    (let ((loop-body-tag (gensym "(loop body)"))
	  (loop-cond-tag (gensym "(loop cond)"))
	  (loop-end-tag (gensym "(loop end)")))
      (rewrite-break-statements loop-end-tag)
      (rewrite-continue-statements loop-cond-tag)
      `((progn ,init)
	,(if post-test-p
	     `(go ,loop-body-tag)		; do-while
	     `(go ,loop-cond-tag))
	,loop-body-tag
	,@body
	,loop-cond-tag
	(when (progn ,cond)
	  (progn ,step)
	  (go ,loop-body-tag))
	,loop-end-tag)))

  (defun push-case-label (exp)
    (let ((case-sym (gensym (format nil "(case ~S)" exp))))
      (setf *case-label-list*
	    (acons case-sym
		   (list :exp exp)
		   *case-label-list*))
      case-sym))

  (defun extract-switch (exp stat)
    (let* ((exp-sym (gensym "(switch exp)"))
	   (end-tag (gensym "(switch end)"))
	   (jump-table			; create jump table
	    (loop with default-clause =`(t (go ,end-tag))
	       for label in *case-label-list*
	       as label-sym = (car label)
	       as label-exp = (getf (cdr label) :exp)

	       if (eq label-exp 'default)
	       do (setf default-clause `(t (go ,label-sym)))
	       else
	       collect `((eql ,exp-sym ,label-exp) (go ,label-sym))
	       into clauses
	       finally
		 (return (append '(cond)
				 clauses
				 `(,default-clause))))))
      (rewrite-break-statements end-tag)
      (push `(,exp-sym) *declarations*)
      (setf *case-label-list* nil)
      `((setf ,exp-sym ,exp)
	,jump-table
	,@stat
	,end-tag)
      ))
)

(define-parser *expression-parser*
  (:start-symbol compound-stat)

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

;; translation_unit	: external_decl
;; 			| translation_unit external_decl
;; 			;
;; external_decl		: function_definition
;; 			| decl
;; 			;
;; function_definition	: decl_specs declarator decl_list compound_stat
;; 			|		declarator decl_list compound_stat
;; 			| decl_specs declarator		compound_stat
;; 			|		declarator 	compound_stat
;; 			;

  ;; TODO: stucks into *declarations*
  (decl
   (decl-specs init-declarator-list \;  ; TODO
               #'(lambda (dcls inits _t)
                   (declare (ignore _t))
                   (list :decl dcls :init inits)))
   (decl-specs \;                       ; TODO
               #'(lambda (dcls _t)
                   (declare (ignore _t))
                   (list :decl dcls))))

  (decl-list
   (decl
    #'list)
   (decl-list decl
              #'(lambda (dcls dcl)
                  (append dcls (list dcl)))))

  ;; returns like:
  ;;   (:type (int) :storage-class (auto register) :qualifier (const))
  (decl-specs
   (storage-class-spec decl-specs
                       #'(lambda (cls dcls)
                           (pushnew cls (getf dcls :storage-class))
                           dcls))
   (storage-class-spec
    #'(lambda (cls)
        (list :storage-class `(,cls))))
   (type-spec decl-specs
              #'(lambda (tp dcls)
                  (pushnew tp (getf dcls :type))
                  dcls))
   (type-spec
    #'(lambda (tp)
        (list :type `(,tp))))
   (type-qualifier decl-specs
                   #'(lambda (qlr dcls)
                       (pushnew qlr (getf dcls :qualifier))
                       dcls))
   (type-qualifier
    #'(lambda (qlr)
        (list :qualifier `(,qlr)))))

  (storage-class-spec
   auto register static extern typedef) ; keywords

  ;; TODO
  (type-spec
   void char short int long float double signed unsigned ; keywords
   struct-or-union-spec
   enum-spec
   typedef-name)                        ; not supported -- TODO!!

  (type-qualifier
   const volatile)                      ; keywords

  ;; TODO
  (struct-or-union-spec
   (struct-or-union id { struct-decl-list })
   (struct-or-union    { struct-decl-list })
   (struct-or-union id))

  ;; TODO
  (struct-or-union
   struct union)                        ; keywords

  ;; TODO
  (struct-decl-list
   struct-decl
   (struct-decl-list struct-decl))

  ;; TODO
  (init-declarator-list
   init-declarator
   (init-declarator-list \, init-declarator))

  ;; TODO
  (init-declarator
   declarator
   (declarator = initializer))

  ;; TODO
  (struct-decl
   (spec-qualifier-list struct-declarator-list \;))

  ;; TODO
  (spec-qualifier-list
   (type-spec spec-qualifier-list)
   type-spec
   (type-qualifier spec-qualifier-list)
   type-qualifier)

  ;; TODO
  (struct-declarator-list
   struct-declarator
   (struct-declarator-list \, struct-declarator))

  ;; TODO
  (struct-declarator
   declarator
   (declarator \: const-exp)
   (\: const-exp))

  ;; TODO                                            
  (enum-spec
   (enum id { enumerator-list })
   (enum    { enumerator-list })
   (enum id))

  ;; TODO
  (enumerator-list
   enumerator
   (enumerator-list \, enumerator))

  ;; TODO
  (enumerator
   id
   (id = const-exp))

  ;; TODO
  (declarator
   (pointer direct-declarator)
   direct-declarator)

  ;; TODO
  (direct-declarator
   id
   (\( declarator \))
   (direct-declarator [ const-exp ])
   (direct-declarator [		  ])
   (direct-declarator \( param-type-list \))
   (direct-declarator \( id-list \))
   (direct-declarator \(	 \)))

  ;; TODO
  (pointer
   (* type-qualifier-list)
   *
   (* type-qualifier-list pointer)
   (*			  pointer))

  ;; TODO
  (type-qualifier-list
   type-qualifier
   (type-qualifier-list type-qualifier))

  ;; TODO
  (param-type-list
   param-list
   (param-list \, |...|))

  ;; TODO
  (param-list
   param-decl
   (param-list \, param-decl))

  ;; TODO
  (param-decl
   (decl-specs declarator)
   (decl-specs abstract-declarator)
   decl-specs)

  ;; TODO
  (id-list
   id
   (id-list \, id))

  ;; TODO
  (initializer
   assignment-exp
   ({ initializer-list })
   ({ initializer-list \, }))

  ;; TODO
  (initializer-list
   initializer
   (initializer-list \, initializer))

  ;; TODO
  (type-name
   (spec-qualifier-list abstract-declarator)
   spec-qualifier-list)

  ;; TODO
  (abstract-declarator
   pointer
   (pointer direct-abstract-declarator)
   direct-abstract-declarator)

  ;; TODO
  (direct-abstract-declarator
   (\( abstract-declarator \))
   (direct-abstract-declarator [ const-exp ])
   (			       [ const-exp ])
   (direct-abstract-declarator [	   ])
   (			       [	   ])
   (direct-abstract-declarator \( param-type-list \))
   (			       \( param-type-list \))
   (direct-abstract-declarator \(		  \))
   (			       \(		  \)))

  ;; ;; TODO
  ;; (typedef-name
  ;;  id)


  ;;; Statements
  (stat
   labeled-stat
   exp-stat 
   compound-stat
   selection-stat
   iteration-stat
   jump-stat)

  (labeled-stat
   (id \: stat
       #'(lambda (id _c stat)
	   (declare (ignore _c))
	   (cons id stat)))
   (case const-exp \: stat
       #'(lambda (_k  exp _c stat)
	   (declare (ignore _k _c))
	   (cons (push-case-label exp)
		 stat)))
   (default \: stat
       #'(lambda (_k _c stat)
	   (declare (ignore _k _c))
	   (cons (push-case-label 'default)
		 stat))))

  (exp-stat
   (exp \;
	#'(lambda (exp term)
	    (declare (ignore term))
	    (list exp)))
   (\;
    #'(lambda (term)
	(declare (ignore term))
	nil)))

  (compound-stat
   ({ decl-list stat-list }
      #'(lambda (_op1 dcls sts _op2)
          (declare (ignore _op1 _op2))
          `(,dcls ,@sts)))             ; TODO
   ({ stat-list }
      #'(lambda (op1 sts op2)
	  (declare (ignore op1 op2))
	  `(,@sts)))
   ({ decl-list	}
      #'(lambda (_op1 dcls _op2)
	  (declare (ignore _op1 _op2))
	  `(,dcls)))                   ; TODO
   ({ }
      #'(lambda (op1 op2)
	  (declare (ignore op1 op2))
	  nil)))

  (stat-list
   (stat
    #'list)
   (stat-list stat
	      #'(lambda (sts st)
		  (append sts (list st)))))

  (selection-stat
   (if \( exp \) stat
       #'(lambda (op lp exp rp stat)
	   (declare (ignore op lp rp))
	   (extract-if-statement exp stat)))
   (if \( exp \) stat else stat
       #'(lambda (op lp exp rp stat1 el stat2)
	   (declare (ignore op lp rp el))
	   (extract-if-statement exp stat1 stat2)))
   (switch \( exp \) stat
	   #'(lambda (_k _lp exp _rp stat)
	       (declare (ignore _k _lp _rp))
	       (extract-switch exp stat))))

  (iteration-stat
   (while \( exp \) stat
	  #'(lambda (_k _lp cond _rp body)
	      (declare (ignore _k _lp _rp))
	      (extract-loop body :cond cond)))
   (do stat while \( exp \) \;
     #'(lambda (_k1 body _k2 _lp cond _rp _t)
	 (declare (ignore _k1 _k2 _lp _rp _t))
	 (extract-loop body :cond cond :post-test-p t)))
   (for \( exp \; exp \; exp \) stat
	#'(lambda (_k _lp init _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :cond cond :step step)))
   (for \( exp \; exp \;     \) stat
	#'(lambda (_k _lp init _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :cond cond)))
   (for \( exp \;     \; exp \) stat
	#'(lambda (_k _lp init _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :step step)))
   (for \( exp \;     \;     \) stat
	#'(lambda (_k _lp init _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init)))
   (for \(     \; exp \; exp \) stat
	#'(lambda (_k _lp      _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :cond cond :step step)))
   (for \(     \; exp \;     \) stat
	#'(lambda (_k _lp      _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :cond cond)))
   (for \(     \;     \; exp \) stat
	#'(lambda (_k _lp      _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :step step)))
   (for \(     \;     \;     \) stat
	#'(lambda (_k _lp      _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body))))

  (jump-stat
   (goto id \;
	 #'(lambda (_k id _t)
	     (declare (ignore _k _t))
	     (list `(go ,id))))
   (continue \;
	     #'(lambda (_k _t)
		 (declare (ignore _k _t))
		 (let ((ret (list 'go (gensym "unresolved continue"))))
		   (push ret *continue-statements*)
		   (list ret))))
   (break \;
	  #'(lambda (_k _t)
	      (declare (ignore _k _t))
	      (let ((ret (list 'go (gensym "unresolved break"))))
		(push ret *break-statements*)
		(list ret))))
   (return exp \;
	   #'(lambda (_k exp _t)
	       (declare (ignore _k _t))
	       (list `(return ,exp))))	; use the block of PROG
   (return \;
	   #'(lambda (_k _t)
	       (declare (ignore _k _t))
	       (list `(return (values)))))) ; use the block of PROG


  ;;; Expressions
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
		    `(,exp ,@args)))
   (postfix-exp \( \)
		#'(lambda (exp op1 op2)
		    (declare (ignore op1 op2))
		    `(,exp)))
   (postfix-exp \. id
		#'(lambda (exp _op id)
		    (declare (ignore _op))
		    `(,id ,exp))) ; id is assumed as a reader
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
   const*
   string
   (\( exp \)
       #'(lambda  (_1 x _3)
	   (declare (ignore _1 _3))
	   x))
   lisp-expression)			; added

  (argument-exp-list
   (assignment-exp
    #'list)
   (argument-exp-list \, assignment-exp
		      #'(lambda (exp1 op exp2)
			  (declare (ignore op))
			  (append exp1 (list exp2)))))

  (const*
   int-const
   char-const
   float-const
   enumeration-const)			; TODO
  )

;; (parse-with-lexer (list-lexer '(x * - - 2 + 3 * y)) *expression-parser*)
;; => (+ (* X (- (- 2))) (* 3 Y))	       

(defun c-expression-tranform (form)
  (let* ((*declarations* nil)
	 (*break-statements* nil)
	 (*continue-statements* nil)
	 (*case-label-list* nil)
	 (ret (parse-with-lexer (list-lexer form)
				*expression-parser*)))
    `(prog ,*declarations*
	,@ret)))

(defmacro with-c-syntax (() &body body)
  (c-expression-tranform body))

#|
(with-c-syntax ()
{
  return 1 + 2 \;
}
)
3

(defparameter x 0)
(with-c-syntax () {
  while \( x < 100 \)
    x ++ \;
  return x \;
}
)

(defparameter i 0)
(with-c-syntax ()
{
  for \( i = 0 \; i < 100 \; ++ i \)
  (format t "~A~%" i) \;
}
)

(defparameter i 0)
(with-c-syntax ()
{
  for \( i = 0 \; i < 100 \; ++ i \) {
    if \( (oddp i) \)
      continue \;
    if \( i == 50 \)
      break \;
    (format t "~A~%" i) \;
  }
}
)

(with-c-syntax ()
{
  switch \( x \) {
  case 1 \:
    (format t "case 1 ni kita~%") \;
    break \;
  case 2 \:
    (format t "case 2 ni kita~%") \;
    (format t "fall-though~%") \;
  default \:
    (format t "default ni kita~%") \;
  }
}
)


(with-c-syntax ()
{
  goto a \;
  a \:
    return 100 \;
}
)

;; removed dereference temporarylly..
(with-c-syntax ()
{
	n = \( count + 7 \) / 8 \;
	switch \( count % 8 \) {
	case 0 \:	do {	to = from ++ \;
	case 7 \:		to = from ++ \;
	case 6 \:		to = from ++ \;
	case 5 \:		to = from ++ \;
	case 4 \:		to = from ++ \;
	case 3 \:		to = from ++ \;
	case 2 \:		to = from ++ \;
	case 1 \:		to = from ++ \;
		} while \( -- n > 0 \) \;
	}
}
)

|#
