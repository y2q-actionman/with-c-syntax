(in-package :with-c-syntax)

;;; Constants
(alexandria:define-constant +operators+
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
      )
  :test 'equal)

(alexandria:define-constant +keywords+
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
      )
  :test 'equal)

;;; Lexer
(defvar *enum-declarations-alist* nil
  "list of (symbol initform)")

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
		     (en (member value *enum-declarations-alist*
				 :key #'car)))
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

;;; Variables, works with the parser.
(defvar *declarations* nil
  "alist of (symbol . initform)")

(defvar *break-statements* nil
  "list of (go 'break), should be rewrited")

(defvar *continue-statements* nil
  "list of (go 'continue), should be rewrited")

(defvar *case-label-list* nil
  "alist of (<gensym> . :exp <case-exp>)")

;;; Functions used by the parser.

;; for declarations 
(defstruct decl-specs
  (type-spec nil)
  (storage-class nil)
  (qualifier nil)
  lisp-type
  lisp-code)

(defstruct init-declarator
  declarator
  (initializer nil))


(defstruct struct-or-union-spec
  type					; symbol. 'struct' or 'union'
  (name nil)
  ;; alist of (spec-qualifier-list . (struct-declarator ...))
  (struct-decl-list nil))

(defstruct (spec-qualifier-list
             (:include decl-specs))
  )

(defstruct struct-declarator
  (name nil)
  (bits nil))


(defstruct enum-spec
  (name nil)
  ;; list of enumerator
  (enumerator-list nil))

(defstruct (enumerator
	     (:include init-declarator))
  )

;; returns two value:
;; - the specified Lisp type
;; - some code, if needed
;; TODO: consider struct-name's scope. If using defclass, it is global!
;; TODO: support cv-qualifier.
(defun lispify-struct-spec (sspec)
  ;; fills name
  (when (null (struct-or-union-spec-name sspec))
    (setf (struct-or-union-spec-name sspec) (gensym "(struct-name)")))
  ;; fields
  (loop for (spec-qual . struct-decls)
     in (struct-or-union-spec-struct-decl-list sspec)
     do (finalize-decl-specs spec-qual)
     collect (decl-specs-lisp-code spec-qual) into codes
     append (loop with tp = (decl-specs-lisp-type spec-qual)
	       for s-decl in struct-decls
	       as name = (or (struct-declarator-name s-decl)
			     (gensym "(unnamed field)"))
	       as bits = (struct-declarator-bits s-decl)
	       collect
		 (let ((tp
			(if bits
			    (cond
			      ((subtypep `(signed-byte ,bits) tp)
			       `(signed-byte ,bits))
			      ((subtypep `(unsigned-byte ,bits) tp)
			       `(unsigned-byte ,bits))
			      (t
			       (error "invalid bitfield: ~A, ~A" tp s-decl)))
			    tp)))
		   `(,(gensym "(struct field slot-name)")
		      :accessor ,name ; TODO: if const, disable writer func
		      :type ,tp
		      :documentation "a slot-accessor generated by with-c-syntax")))
     into fields
     finally
       (return (values (struct-or-union-spec-name sspec)
		       `(progn
			  (defclass ,(struct-or-union-spec-name sspec) ()
			    ,fields
			    (:documentation "a class generated by with-c-syntax"))
			  ,@codes)))))

;; returns two value:
;; - the specified Lisp type
;; - some code, if needed
;; changes *enum-declarations-alist*
;; TODO: consider enum-name's scope. If using deftype, it is global!
(defun lispify-enum-spec (espec)
  ;; fills name
  (when (null (enum-spec-name espec))
    (setf (enum-spec-name espec) (gensym "(enum-name)")))
  ;; addes values into *declarations*
  (loop as default-initform = 0 then `(1+ ,(init-declarator-declarator e))
     for e in (enum-spec-enumerator-list espec)
     collect (list (init-declarator-declarator e)
		   (or (init-declarator-initializer e) default-initform))
     into edecls
     finally (setf *enum-declarations-alist*
		   (append *enum-declarations-alist* edecls)))
  (values (enum-spec-name espec)
	  `(deftype ,(enum-spec-name espec) ()
	     'fixnum)))

;; returns two value:
;; - the specified Lisp type
;; - ~defstruct code~, if needed
(defun lispify-type-spec (tp-list)
  (loop with numeric-type = nil 
     with variant-table = (make-hash-table)

     for tp in tp-list

     if (eq tp 'void)			; void
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (return nil)

     else if (struct-or-union-spec-p tp) ; struct / union
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (return (lispify-struct-spec tp))

     else if (enum-spec-p tp)	; enum
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (return (lispify-enum-spec tp))

     ;; numeric types
     else if (member tp '(float double int char))
     do (when numeric-type
	  (error "invalid decl-spec (~A)" tp-list))
       (setf numeric-type tp)
     ;; numeric variants
     else if (member tp '(signed unsigned long short))
     do (incf (gethash tp variant-table 0))
     else
     do (assert nil)
       
     finally
       (ecase numeric-type
	 (float
	  (when (or (gethash 'signed variant-table)
		    (gethash 'unsigned variant-table)
		    (gethash 'long variant-table)
		    (<= 2 (gethash 'short variant-table 0)))
	    (error "invalid decl-spec (~A)" tp-list))
	  (return (if (gethash 'short variant-table)
		       'short-float 'single-float)))
	 (double
	  (when (or (gethash 'signed variant-table)
		    (gethash 'unsigned variant-table)
		    (gethash 'short variant-table)
		    (<= 2 (gethash 'long variant-table 0)))
	    (error "invalid decl-spec (~A)" tp-list))
	  (return (if (gethash 'long variant-table)
                      'long-float 'double-float)))
	 (char
	  (when (or (gethash 'short variant-table)
		    (gethash 'long variant-table)
		    (and (gethash 'signed variant-table)
			 (gethash 'unsigned variant-table)))
	    (error "invalid decl-spec (~A)" tp-list))
	  (return (if (gethash 'unsigned variant-table)
		       '(unsigned-byte 8)
		       '(signed-byte 8))))
	 ((int nil)
	  (when (or (and (gethash 'signed variant-table)
			 (gethash 'unsigned variant-table))
		    (and (gethash 'short variant-table)
			 (gethash 'long variant-table))
		    (<= 2 (gethash 'short variant-table 0))
		    (<= 3 (gethash 'long variant-table 0)))
	    (error "invalid decl-spec (~A)" tp-list))
	  ;; TODO: supply accurate bit range..
	  (return (if (gethash 'long variant-table)
                      'integer
                      'fixnum))))))

(defun finalize-decl-specs (dspecs)
  (multiple-value-bind (ltype lcode)
      (lispify-type-spec (decl-specs-type-spec dspecs))
    (setf (decl-specs-lisp-type dspecs) ltype
	  (decl-specs-lisp-code dspecs) lcode))
  (setf (decl-specs-qualifier dspecs)
	(remove-duplicates (decl-specs-qualifier dspecs)))
  (when (> (length (decl-specs-storage-class dspecs)) 1)
    (error "too many storage-class specified: ~A"
	   (decl-specs-storage-class dspecs)))
  (setf (decl-specs-storage-class dspecs)
	(first (decl-specs-storage-class dspecs)))
  dspecs)

;; TODO
(defun lispify-declaration (decl inits)
  )

;; for expressions
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; These are directly called by the parser..
(defun lispify-unary (op)
  #'(lambda (_ exp)
      (declare (ignore _))
      `(,op ,exp)))

(defun lispify-binary (op)
  #'(lambda (exp1 _ exp2)
      (declare (ignore _))
      `(,op ,exp1 ,exp2)))

(defun lispify-post-increment (op)
  #'(lambda (exp _)
      (declare (ignore _))
      (let ((tmp (gensym)))
	`(let ((,tmp ,exp))
	   (setf ,exp (,op ,tmp 1))
	   ,tmp))))

(defun lispify-augmented-assignment (op)
  #'(lambda (exp1 _ exp2)
      (declare (ignore _))
      (let ((tmp (gensym)))
	`(let ((,tmp ,exp1))
	   (setf ,exp1
		 (,op ,tmp ,exp2))))))
)

(defun ash-right (i c)
  (ash i (- c)))

;; for statements
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

(defvar *unresolved-break-tag* (gensym "unresolved break"))

(defun allocate-unresolved-break-statement ()
  (let ((ret (list 'go *unresolved-break-tag*)))
    (push ret *break-statements*)
    (list ret)))

(defun rewrite-break-statements (sym)
  (loop for i in *break-statements*
     do (setf (second i) sym))
  (setf *break-statements* nil))

(defvar *unresolved-continue-tag* (gensym "unresolved continue"))

(defun allocate-unresolved-continue-statement ()
  (let ((ret (list 'go *unresolved-continue-tag*)))
    (push ret *continue-statements*)
    (list ret)))

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
  (let* ((exp-sym (gensym "(switch cond)"))
	 (end-tag (gensym "(switch end)"))
	 (jump-table			; create jump table with COND
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

;;; Functions referenced by the parser directly.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append-item-to-right (lis i)
    (append lis (list i)))

  (defun concatinate-comma-list (lis op i)
    (declare (ignore op))
    (append-item-to-right lis i))
)

;;; The parser
(define-parser *expression-parser*
  (:muffle-conflicts t)

  (:start-symbol w-c-s-entry-point)

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

  ;; Our entry point.
  ;; top level forms in C, or statements
  (w-c-s-entry-point
   translation-unit
   labeled-stat
   ;; exp-stat is not included, because it is gramatically ambiguous.
   compound-stat
   selection-stat
   iteration-stat
   jump-stat)

  (translation-unit
   (external-decl
    #'list)
   (translation-unit external-decl
    #'append-item-to-right))

  (external-decl
   function-definition
   decl)

  (function-definition
   (decl-specs declarator decl-list compound-stat
    #'(lambda (ret name k&r-decls body)
	(list :function-definition
	      :return ret
	      :name name
	      :K&R-style-decls k&r-decls
	      :body body)))
   (           declarator decl-list compound-stat
    #'(lambda (name k&r-decls body)
	(list :function-definition
	      :return nil
	      :name name
	      :K&R-style-decls k&r-decls
	      :body body)))
   (decl-specs declarator           compound-stat
    #'(lambda (ret name body)
	(list :function-definition
	      :return ret
	      :name name
	      ;; :K&R-style-decls nil
	      :body body)))
   (           declarator           compound-stat
    #'(lambda (name body)
	(list :function-definition
	      :return nil
	      :name name
	      ;; :K&R-style-decls nil
	      :body body))))

  ;; TODO: stucks into *declarations*
  (decl
   (decl-specs init-declarator-list \;	; TODO
               #'(lambda (dcls inits _t)
                   (declare (ignore _t))
		   (setf dcls (finalize-decl-specs dcls))
		   (lispify-declaration dcls inits)))
   (decl-specs \;
               #'(lambda (dcls _t)
                   (declare (ignore _t))
		   (setf dcls (finalize-decl-specs dcls))
		   (decl-specs-lisp-code dcls))))

  (decl-list
   (decl
    #'list)
   (decl-list decl
	      #'append-item-to-right))

  ;; returns 'decl-specs' structure
  (decl-specs
   (storage-class-spec decl-specs
                       #'(lambda (cls dcls)
                           (push cls (decl-specs-storage-class dcls))
                           dcls))
   (storage-class-spec
    #'(lambda (cls)
	(make-decl-specs :storage-class `(,cls))))
   (type-spec decl-specs
              #'(lambda (tp dcls)
                  (push tp (decl-specs-type-spec dcls))
                  dcls))
   (type-spec
    #'(lambda (tp)
	(make-decl-specs :type-spec `(,tp))))
   (type-qualifier decl-specs
                   #'(lambda (qlr dcls)
                       (push qlr (decl-specs-qualifier dcls))
                       dcls))
   (type-qualifier
    #'(lambda (qlr)
	(make-decl-specs :qualifier `(,qlr)))))

  (storage-class-spec
   auto register static extern typedef) ; keywords

  (type-spec
   void char short int long float double signed unsigned ; keywords
   struct-or-union-spec                                  ; TODO
   enum-spec                                             ; TODO
   typedef-name)                        ; not supported -- TODO!!

  (type-qualifier
   const volatile)                      ; keywords

  ;; returns struct-or-union-spec structure
  (struct-or-union-spec
   (struct-or-union id { struct-decl-list }
                    #'(lambda (kwd id _l decl _r)
                        (declare (ignore _l _r))
			(make-struct-or-union-spec
			 :type kwd :name id :struct-decl-list decl)))
   (struct-or-union    { struct-decl-list }
                    #'(lambda (kwd _l decl _r)
                        (declare (ignore _l _r))
			(make-struct-or-union-spec
			 :type kwd :struct-decl-list decl)))
   (struct-or-union id
                    #'(lambda (kwd id)
			(make-struct-or-union-spec
			 :type kwd :name id))))

  (struct-or-union
   struct union)                        ; keywords

  (struct-decl-list
   (struct-decl
    #'list)
   (struct-decl-list struct-decl
		     #'append-item-to-right))

  (init-declarator-list
   (init-declarator
    #'list)
   (init-declarator-list \, init-declarator
                         #'concatinate-comma-list))

  ;; returns init-declarator structure
  (init-declarator
   (declarator
    #'(lambda (d)
	(make-init-declarator :declarator d)))
   (declarator = initializer
               #'(lambda (d _op i)
                   (declare (ignore _op))
		   (make-init-declarator :declarator d
					 :initializer i))))

  ;; returns (spec-qualifier-list . struct-declarator-list)
  (struct-decl
   (spec-qualifier-list struct-declarator-list \;
                        #'(lambda (qls dcls _t)
                            (declare (ignore _t))
			    (cons qls dcls))))

  ;; returns spec-qualifier-list structure
  (spec-qualifier-list
   (type-spec spec-qualifier-list
	      #'(lambda (tp lis)
		  (push tp (spec-qualifier-list-type-spec lis))
		  lis))
   (type-spec
    #'(lambda (tp)
	(make-spec-qualifier-list :type-spec `(,tp))))
   (type-qualifier spec-qualifier-list
		   #'(lambda (ql lis)
		       (push ql (spec-qualifier-list-qualifier lis))
		       lis))
   (type-qualifier
    #'(lambda (ql)
	(make-spec-qualifier-list :qualifier `(,ql)))))

  (struct-declarator-list
   (struct-declarator
    #'list)
   (struct-declarator-list \, struct-declarator
			   #'concatinate-comma-list))

  ;; returns struct-declarator structure
  (struct-declarator
   (declarator
    #'(lambda (d)
	(make-struct-declarator :name d)))
   (declarator \: const-exp
	       #'(lambda (d _c bits)
		   (declare (ignore _c))
		   (make-struct-declarator :name d :bits bits)))
   (\: const-exp
       #'(lambda (_c bits)
	   (declare (ignore _c))
	   (make-struct-declarator :bits bits))))

  ;; returns enum-spec structure
  (enum-spec
   (enum id { enumerator-list }
         #'(lambda (_kwd id _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :name id :enumerator-list lis)))
   (enum    { enumerator-list }
         #'(lambda (_kwd _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :enumerator-list lis)))
   (enum id
         #'(lambda (_kwd id)
             (declare (ignore _kwd))
	     (make-enum-spec :name id))))

  (enumerator-list
   (enumerator
    #'list)
   (enumerator-list \, enumerator
                    #'concatinate-comma-list))

  ;; returns enumerator structure
  (enumerator
   (id
    #'(lambda (id)
	(make-enumerator :declarator id)))
   (id = const-exp
       #'(lambda (id _op exp)
           (declare (ignore _op))
	   (make-enumerator :declarator id :initializer exp))))

  ;; uses directly..
  (declarator
   (pointer direct-declarator)
   direct-declarator)

  ;; see direct-abstract-declarator
  ;; TODO: introduce some structure?
  (direct-declarator
   id
   (\( declarator \)
    #'(lambda (_lp dcl _rp)
	(declare (ignore _lp _rp))
	`(:function-pointer ,dcl)))
   (direct-declarator [ const-exp ]
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
	`(,dcl :aref ,params)))
   (direct-declarator [		  ]
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
	`(,dcl :aref nil)))
   (direct-declarator \( param-type-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
	`(,dcl :funcall ,params)))
   (direct-declarator \( id-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
	`(,dcl :funcall ,params)))	; TODO: we should see type-spec or not.
   (direct-declarator \(	 \)
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
	`(,dcl :funcall nil))))

  ;; returns like:
  ;; (*), (* *), (* * *)
  ;; ((* const)), (* (* const)), (* (* const) *)
  ;; TODO: introduce some structure?
  (pointer
   (* type-qualifier-list
      #'(lambda (kwd qls)
	  `((,kwd ,@qls))))
   (*
    #'(lambda (kwd)
	(list kwd)))
   (* type-qualifier-list pointer
      #'(lambda (kwd qls p)
	  `((,kwd ,@qls) ,@p)))
   (*			  pointer
      #'(lambda (kwd p)
	  `(,kwd ,@p))))
			  

  (type-qualifier-list
   (type-qualifier
    #'list)
   (type-qualifier-list type-qualifier
			#'append-item-to-right))

  (param-type-list
   param-list
   (param-list \, |...|
	       #'concatinate-comma-list))

  (param-list
   (param-decl
    #'list)
   (param-list \, param-decl
	       #'concatinate-comma-list))

  ;; TODO: introduce some structure?
  (param-decl
   (decl-specs declarator
	       #'(lambda (dls abs)
		   `(,@dls :suffixes ,abs))) ; TODO
   (decl-specs abstract-declarator
	       #'(lambda (dls abs)
		   `(,@dls :suffixes ,abs)))
   decl-specs)

  (id-list
   (id
    #'list)
   (id-list \, id
    #'concatinate-comma-list))

  ;; TODO: introduce some structure?
  (initializer
   assignment-exp
   ({ initializer-list }
    #'(lambda (_lp inits _rp)
	(declare (ignore _lp _rp))
	inits))
   ({ initializer-list \, }
    #'(lambda (_lp inits _cm _rp)
	(declare (ignore _lp _cm _rp))
	inits)))			; TODO: see again..

  (initializer-list
   (initializer
    #'list)
   (initializer-list \, initializer
    #'concatinate-comma-list))

  ;; TODO: introduce some struct
  (type-name
   (spec-qualifier-list abstract-declarator
			#'(lambda (qls abs)
			    `(,@qls :suffixes ,abs)))
   (spec-qualifier-list
    #'identity))

  ;; TODO: introduce some structure?
  (abstract-declarator
   pointer
   (pointer direct-abstract-declarator)
   direct-abstract-declarator)

  ;; returns like:
  ;; (:aref nil) (:funcall nil) (:aref 5 :funcall (int))
  ;; TODO: introduce some structure?
  (direct-abstract-declarator
   (\( abstract-declarator \)
    #'(lambda (_lp dcls _rp)
	(declare (ignore _lp _rp))
	`(:function-pointer ,@dcls)))
   (direct-abstract-declarator [ const-exp ]
    #'(lambda (dcls _lp params _rp)
	(declare (ignore _lp _rp))
	`(,@dcls :aref ,params)))
   (			       [ const-exp ]
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
	`(:aref ,params)))
   (direct-abstract-declarator [	   ]
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
	`(,@dcls :aref nil)))
   (			       [	   ]
    #'(lambda (_lp _rp)
	(declare (ignore _lp _rp))
	`(:aref nil)))
   (direct-abstract-declarator \( param-type-list \)
    #'(lambda (dcls _lp params _rp)
	(declare (ignore _lp _rp))
	`(,@dcls :funcall ,params)))
   (			       \( param-type-list \)
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
	`(:funcall ,params)))
   (direct-abstract-declarator \(		  \)
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
	`(,@dcls :funcall nil)))
   (			       \(		  \)
    #'(lambda (_lp _rp)
	(declare (ignore _lp _rp))
	`(:funcall nil))))
						  

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
          `(,@dcls ,@(apply #'append sts)))) ; FIXME: looks confused..
   ({ stat-list }
      #'(lambda (op1 sts op2)
	  (declare (ignore op1 op2))
	  (apply #'append sts)))	; flatten
   ({ decl-list	}
      #'(lambda (_op1 dcls _op2)
	  (declare (ignore _op1 _op2))
	  `(,@dcls)))			; FIXME: looks confused..
   ({ }
      #'(lambda (op1 op2)
	  (declare (ignore op1 op2))
	  nil)))

  (stat-list
   (stat
    #'list)
   (stat-list stat
	      #'append-item-to-right))

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
		 (allocate-unresolved-continue-statement)))
   (break \;
	  #'(lambda (_k _t)
	      (declare (ignore _k _t))
	      (allocate-unresolved-break-statement)))
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
   (& cast-exp
      #'(lambda (_op exp)
          (declare (ignore _op))
          ;; TODO: consider it. We should this exp is setf-able or not?
          (if (symbolp exp)
              `(make-pseudo-pointer* ,exp ',exp)
              `(make-pseudo-pointer ,exp))))a
   (* cast-exp
      #'(lambda (_op exp)
          (declare (ignore _op))
          `(pseudo-pointer-dereference ,exp)))
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
		#'(lambda (exp _op id) ; id is assumed as a reader
		    (declare (ignore _op))
		    `(,id ,exp)))
   (postfix-exp -> id
		#'(lambda (exp _op id) ; id is assumed as a reader
		    (declare (ignore _op))
		    `(,id (pseudo-pointer-dereference ,exp))))
   (postfix-exp ++
                (lispify-post-increment '+))
   (postfix-exp --
                (lispify-post-increment '-)))

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
                      #'concatinate-comma-list))

  (const*
   int-const
   char-const
   float-const
   enumeration-const)			; TODO
  )

;;; Expander
(defun c-expression-tranform (refering-symbols form)
  (let* ((*enum-declarations-alist* nil)
	 (*declarations* nil)
	 (*break-statements* nil)
	 (*continue-statements* nil)
	 (*case-label-list* nil)
	 (lisp-exp (parse-with-lexer (list-lexer form)
                                     *expression-parser*))
         (dynamic-syms nil)
         (dynamic-vals nil)
	 (lexical-binds nil))
    (loop for s in refering-symbols
       do (cond ((symbolp s)
                 (push s dynamic-syms)
                 (push nil dynamic-vals))
                ((listp s)
                 (push (first s) dynamic-syms)
                 (push (second s) dynamic-vals))))
    ;; expand declarations
    (loop for i in *declarations*
       do (push (car i) dynamic-syms)
          (push (cdr i) dynamic-vals))
    (setf lexical-binds
	  (append lexical-binds *enum-declarations-alist*))
    ;; TODO: muffle undefined-variable warnings
    ;; TODO: support function definition. This is only for a compound statement.
    `(with-pseudo-pointer-scope ()
       (progv ',dynamic-syms (list ,@dynamic-vals)
         (locally (declare (special ,@dynamic-syms))
           (prog* ,lexical-binds
              ,@lisp-exp))))))

;;; Macro interface
(defmacro with-c-syntax ((&rest bindings) &body body)
  (c-expression-tranform bindings body))
