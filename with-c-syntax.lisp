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

;;; Variables
(defvar *enum-const-symbols* nil
  "list of (symbol initform)")
(defvar *toplevel-bindings* nil)
(defvar *dynamic-binding-requested* nil)
(defvar *w-c-s-structs* nil
  "hashtable: struct-name -> list of w-c-s-struct-spec.
If a same name is supplied, it is stacked")

(defmacro with-new-w-c-s-environment (() &body body)
  `(let* ((*enum-const-symbols* nil)
          (*toplevel-bindings* bindings)
          (*dynamic-binding-requested* nil)
          (*w-c-s-structs* (make-hash-table :test 'eq)))
     ,@body))

;;; Lexer
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
		     (en (member value *enum-const-symbols*)))
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

;;; Declarations
(defstruct decl-specs
  (type-spec nil)
  (storage-class nil)
  (qualifier nil)
  lisp-type                             ; typename for Common Lisp
  (w-c-s-type-tag nil)			; user supplied struct name (or enum name)
  (lisp-bindings nil)			; enum
  (lisp-class-spec nil))                ; w-c-s-struct-spec

(defstruct init-declarator
  declarator
  (initializer nil)
  (lisp-name)
  (lisp-initform)
  (lisp-type))

(defstruct struct-or-union-spec
  type					; symbol. 'struct' or 'union'
  (id nil)
  ;; alist of (spec-qualifier-list . (struct-declarator ...))
  (struct-decl-list nil))

(defstruct (spec-qualifier-list
             (:include decl-specs))
  )

(defstruct (struct-declarator
             (:include init-declarator))
  (bits nil))


(defstruct enum-spec
  (id nil)				; enum tag
  ;; list of enumerator
  (enumerator-list nil))

(defstruct (enumerator
	     (:include init-declarator))
  )

(defvar *default-decl-specs*
  (make-decl-specs :type-spec '(int)
		   :lisp-type 'fixnum))

;; structure information
(defstruct w-c-s-struct-spec
  struct-name                           ; user supplied name (symbol)
  internal-name                         ; internal name (gensym)
  struct-type                           ; 'struct or 'union
  ;; (:lisp-type ... :constness ... :decl-specs ...)
  slot-defs)

(defun ensure-w-c-s-struct-spec (name)
  (let ((wcsspec (first (gethash name *w-c-s-structs*))))
    (unless wcsspec
      (setf wcsspec
            (make-w-c-s-struct-spec
             :struct-name name
             :internal-name (gensym (format nil "struct ~S " name))))
      (push wcsspec (gethash name *w-c-s-structs*)))
    wcsspec))

(defun find-w-c-s-struct-spec (name assumed-internal-name)
  (let ((wcsspec (first (gethash name *w-c-s-structs*))))
    (when (null wcsspec)
      (error "struct ~S is not defined" name))
    (unless (eq (w-c-s-struct-spec-internal-name wcsspec)
                assumed-internal-name)
      (error "internal error: struct spec is confused: ~S" name))
    wcsspec))

(defun drop-w-c-s-struct-spec (wcsspec)
  (let ((name (w-c-s-struct-spec-struct-name wcsspec)))
    (symbol-macrolet ((place (gethash name *w-c-s-structs*)))
      (setf place
            (remove wcsspec place :key #'w-c-s-struct-spec-internal-name)))))

(defun w-c-s-struct-initarg-symbol (index)
  (intern (format nil "~D" index)))

(defun maybe-w-c-s-struct-internal-name-p (obj)
  (and (symbolp obj) (not (symbol-package obj))))

;; processes structure-spec 
(defun finalize-struct-spec (sspec dspecs)
  (let* ((wcsname (or (struct-or-union-spec-id sspec)
                      (gensym "unnamed-struct")))
         (wcsspec (ensure-w-c-s-struct-spec wcsname)))
    (setf (decl-specs-w-c-s-type-tag dspecs) wcsname)
    (setf (decl-specs-lisp-type dspecs)
          (w-c-s-struct-spec-internal-name wcsspec))
    ;; only declaration?
    (when (null (struct-or-union-spec-struct-decl-list sspec))
      (assert (struct-or-union-spec-id sspec)) ; this case is rejected by the parser.
      (return-from finalize-struct-spec dspecs))
    ;; Doubly defined?
    (when (and (struct-or-union-spec-struct-decl-list sspec)
               (w-c-s-struct-spec-slot-defs wcsspec))
      (error "struct is defined twice (~S)" wcsname))
    ;; Now defines a new struct.
    (setf (w-c-s-struct-spec-struct-type wcsspec)
          (struct-or-union-spec-type sspec))
    (loop for (spec-qual . struct-decls)
       in (struct-or-union-spec-struct-decl-list sspec)
       do (finalize-decl-specs spec-qual)
       ;; other struct
       do (appendf (decl-specs-lisp-bindings dspecs)
                   (decl-specs-lisp-bindings spec-qual))
         (appendf (decl-specs-lisp-class-spec dspecs)
                  (decl-specs-lisp-class-spec spec-qual))
       ;; this struct
       nconc
         (loop with tp = (decl-specs-lisp-type spec-qual)
            with constness = (member 'const (decl-specs-qualifier spec-qual))
            for s-decl in struct-decls
            as (decl-name . abst-decl) = (init-declarator-declarator s-decl)
            as name = (or decl-name (gensym "(unnamed field)"))
            as initform = (expand-init-declarator-init spec-qual abst-decl nil)
            as bits = (struct-declarator-bits s-decl)
            if (and bits
                    (not (subtypep `(signed-byte ,bits) tp))
                    (not (subtypep `(unsigned-byte ,bits) tp)))
            ;; NOTE: In C, max bits are limited to the normal type.
            ;; http://stackoverflow.com/questions/2647320/struct-bitfield-max-size-c99-c
            do (error "invalid bitfield: ~A, ~A" tp s-decl) ; limit bits.
            collect (list :lisp-type tp :constness constness
                          :name name
                          ;; If form is too complex, it is rest as unbound.
                          :initform (if (constantp initform) initform nil)
                          :decl-specs spec-qual))
       into slot-specs
       finally
         (setf (w-c-s-struct-spec-slot-defs wcsspec) slot-specs))
    ;; This wcsspec is treated by this dspecs
    (append-item-to-right-f (decl-specs-lisp-class-spec dspecs)
                            wcsspec)
    dspecs))

;; processes enum-spec 
(defun finalize-enum-spec (espec dspecs)
  (setf (decl-specs-lisp-type dspecs) 'fixnum)
  (setf (decl-specs-w-c-s-type-tag dspecs)
	(or (enum-spec-id espec) (gensym "unnamed-enum")))
  ;; addes values into lisp-decls
  (setf (decl-specs-lisp-bindings dspecs)
	(loop as default-initform = 0 then `(1+ ,e-decl)
	   for e in (enum-spec-enumerator-list espec)
	   as e-decl = (init-declarator-declarator e)
	   as e-init = (init-declarator-initializer e)
           do (push e-decl *enum-const-symbols*)
	   collect (list e-decl (or e-init default-initform))))
  dspecs)

(defun finalize-type-spec (dspecs)
  (loop with numeric-type = nil 
     with numeric-signedness = nil	; 'signed, 'unsigned, or nil
     with numeric-length = 0		; -1(short), 1(long), 2(long long), or 0
     with tp-list = (decl-specs-type-spec dspecs)
       
     for tp in tp-list

     if (eq tp 'void)			; void
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (setf (decl-specs-lisp-type dspecs) nil)
       (return dspecs)

     else if (struct-or-union-spec-p tp) ; struct / union
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (return (finalize-struct-spec tp dspecs))

     else if (enum-spec-p tp)	; enum
     do (unless (= 1 (length tp-list))
	  (error "invalid decl-spec (~A)" tp-list))
       (return (finalize-enum-spec tp dspecs))

     ;; numeric types
     else if (member tp '(float double int char))
     do (when numeric-type
	  (error "invalid decl-spec (~A)" tp-list))
       (setf numeric-type tp)
     ;; numeric variants
     else if (member tp '(signed unsigned))
     do (when numeric-signedness
	  (error "invalid decl-spec (~A)" tp-list))
       (setf numeric-signedness tp)
     else if (eq tp 'long)
     do (unless (<= 0 numeric-length 1)
	  (error "invalid decl-spec (~A)" tp-list))
       (incf numeric-length)
     else if (eq tp 'short)
     do (unless (= 0 numeric-length)
	  (error "invalid decl-spec (~A)" tp-list))
       (decf numeric-length)

     else
     do (assert nil)
       
     finally
       (setf (decl-specs-lisp-type dspecs)
	     (ecase numeric-type
	       (float
		(when (or numeric-signedness
			  (not (member numeric-length '(-1 0))))
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq numeric-length -1)
		    'short-float 'single-float))
	       (double
		(when (or numeric-signedness
			  (not (member numeric-length '(0 1))))
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq numeric-length 1)
		    'long-float 'double-float))
	       (char
		(when (not (= 0 numeric-length))
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq 'unsigned numeric-signedness) ; raw 'char' is signed
		    '(unsigned-byte 8)
		    '(signed-byte 8)))
	       ((int nil)
		(if (eq 'unsigned numeric-signedness)
		    (ecase numeric-length
		      (2 '(unsigned-byte 64))
		      (1 '(unsigned-byte 32))
		      (0 'fixnum)	; FIXME: consider unsigned?
		      (-1 '(unsigned-byte 16)))
		    (ecase numeric-length
		      (2 '(signed-byte 64))
		      (1 '(signed-byte 32))
		      (0 'fixnum)
		      (-1 '(signed-byte 16)))))))
       (return dspecs)))

(defun finalize-decl-specs (dspecs)
  (finalize-type-spec dspecs)
  (setf (decl-specs-qualifier dspecs)
	(remove-duplicates (decl-specs-qualifier dspecs)))
  (setf (decl-specs-storage-class dspecs)
	(if (> (length (decl-specs-storage-class dspecs)) 1)
	    (error "too many storage-class specified: ~A"
		   (decl-specs-storage-class dspecs))
	    (first (decl-specs-storage-class dspecs))))
  dspecs)

(defun array-dimension-combine (array-dimension-list init)
  (loop with init-dims = (dimension-list-max-dimensions init)
     for depth from 0
     for a-elem in array-dimension-list
     as i-elem = (nth depth init-dims)
     if (null i-elem)
     collect a-elem
     else if (eq a-elem '*)
     collect i-elem
     else if (<= i-elem a-elem)
     collect a-elem
     else
     do (warn "too much elements in an initializer (~S, ~S)"
              array-dimension-list init)
     and collect a-elem))

(defun setup-init-list (dims dspecs abst-declarator init)
  (let* ((default (expand-init-declarator-init dspecs
                   (nthcdr (length dims) abst-declarator)
                   nil))
         (ret (make-dimension-list dims default)))
    (labels ((var-init-setup (rest-dims rev-aref abst-decls init)
               (if (null rest-dims)
                   (setf (apply #'ref-dimension-list ret (reverse rev-aref))
                         (expand-init-declarator-init dspecs abst-decls init))
                   (loop for d from 0 below (car rest-dims)
                      for init-i in init
                      do (assert (eq :aref (first (car abst-decls))))
                      do (var-init-setup (cdr rest-dims) (cons d rev-aref)
                                         (cdr abst-decls) init-i)))))
      (var-init-setup dims () abst-declarator init))
    ret))

;; returns (values var-init var-type)
(defun expand-init-declarator-init (dspecs abst-declarator initializer)
  (ecase (car (first abst-declarator))
    (:pointer
     ;; TODO: includes 'what it points'
     (values (or initializer 0) 'pseudo-pointer))
    (:funcall
     (when (eq :aref (car (second abst-declarator)))
       (error "a function returning an array is not accepted"))
     (when (eq :funcall (car (second abst-declarator)))
       (error "a function returning a function is not accepted"))
     (unless (null initializer)
       (error "a function cannot take a initializer"))
     ;; TODO: includes returning type, and arg type
     (values nil 'function))
    (:aref
     (let* ((aref-type (decl-specs-lisp-type dspecs))
            (aref-dim                   ; reads abst-declarator
             (loop for (tp tp-args) in abst-declarator
                if (eq :funcall tp)
                do (error "an array of functions is not accepted")
                else if (eq :aref tp)
                collect (or tp-args '*)
                else if (eq :pointer tp)
                do (setf aref-type 'pseudo-pointer) (loop-finish)
                else
                do (assert nil)))
            (merged-dim
             (array-dimension-combine aref-dim initializer))
            (lisp-elem-type
             (if (subtypep aref-type 'number) aref-type t)) ; excludes compound types
            (var-type
             (progn
               (when (and (or (null aref-dim)
                              (member '* aref-dim))
                          (null initializer))
                 (error "array's dimension cannot be specified (~S, ~S)"
                        aref-dim initializer))
               `(simple-array ,lisp-elem-type ,merged-dim)))
            (init-list
             (setup-init-list merged-dim dspecs
                              abst-declarator initializer))
            (var-init
             `(make-array ',merged-dim
                          :element-type ',lisp-elem-type
                          :initial-contents
                          ,(make-dimension-list-load-form init-list (length merged-dim)))))
       (values var-init var-type)))
    ((nil)
     (let ((var-type (decl-specs-lisp-type dspecs)))
       (when (null var-type)
         (error "a void variable cannot be initialized"))
       (cond
         ((subtypep var-type 'number) ; includes enum
          (values (or initializer 0) var-type))
         ((maybe-w-c-s-struct-internal-name-p var-type)
          (let* ((wcs-tag (decl-specs-w-c-s-type-tag dspecs))
                  ;; This is required, for treating a struct defined after declarations.
                 (wcsspec (find-w-c-s-struct-spec wcs-tag var-type))
                 (init-list
                  (loop for idx from 0
                     for i in initializer
                     for slot in (w-c-s-struct-spec-slot-defs wcsspec)
                     collect `(quote ,(w-c-s-struct-initarg-symbol idx))
                     collect (expand-init-declarator-init
                              (getf slot :decl-specs) (cdr abst-declarator) i))))
            (values `(make-instance
                      ',(w-c-s-struct-spec-internal-name wcsspec)
                      ,@init-list)
                    var-type)))
         (t (error "Internal error: unknown type ~S" var-type)))))))

(defun finalize-init-declarator (dspecs init-decl)
  (let* ((decl (init-declarator-declarator init-decl))
         (init (init-declarator-initializer init-decl))
         (var-name (first decl))
	 (storage-class (decl-specs-storage-class dspecs)))
    (when (and init
	       (eq 'extern storage-class))
      (error "an extern variable cannot have any initializers"))
    (when (and (eq :funcall (car (second decl)))
	       (not (member storage-class '(nil extern))))
      (error "a function cannot have storage-class except 'extern'"))
    (multiple-value-bind (var-init var-type)
	(expand-init-declarator-init dspecs (cdr decl) init)
      (setf (init-declarator-lisp-name init-decl) var-name
	    (init-declarator-lisp-initform init-decl) var-init
	    (init-declarator-lisp-type init-decl) var-type)
    init-decl)))

;;; Expressions
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

(defun lispify-type-name (qls abs)
  (setf qls (finalize-decl-specs qls))
  (if abs
      (let ((init-decl (make-init-declarator :declarator abs)))
	(setf init-decl (finalize-init-declarator qls init-decl))
        (init-declarator-lisp-type init-decl))
      (decl-specs-lisp-type qls)))


;;; Statements
(defstruct stat
  (code nil)
  (declarations nil)        ; list of 'init-declarator'
  (break-statements nil)    ; list of (go 'break), should be rewrited
  (continue-statements nil) ; list of (go 'continue), should be rewrited
  (case-label-list nil))    ; alist of (<gensym> . :exp <case-exp>)

(defun merge-stat (s1 s2 &key (merge-code nil))
  (make-stat :code (if merge-code (append (stat-code s1)
					  (stat-code s2))
		       nil)
	     :declarations (append (stat-declarations s1)
				   (stat-declarations s2))
	     :break-statements (append (stat-break-statements s1)
				       (stat-break-statements s2))
	     :continue-statements (append (stat-continue-statements s1)
					  (stat-continue-statements s2))
	     :case-label-list (append (stat-case-label-list s1)
				      (stat-case-label-list s2))))

(defun extract-if-statement (exp then-stat
			     &optional (else-stat nil))
  (let* ((stat (if else-stat
		   (merge-stat then-stat else-stat)
		   then-stat))
	 (then-tag (gensym "(if then)"))
	 (else-tag (gensym "(if else)"))
	 (end-tag (gensym "(if end)")))
    (setf (stat-code stat)
	  `((if ,exp (go ,then-tag) (go ,else-tag))
	    ,then-tag
	    ,@(stat-code then-stat)
	    (go ,end-tag)
	    ,else-tag
	    ,@(if else-stat (stat-code else-stat) nil)
	    (go ,end-tag)
	    ,end-tag))
    stat))

(defvar *unresolved-break-tag* (gensym "unresolved break"))

(defun make-stat-unresolved-break ()
  (let ((ret (list 'go *unresolved-break-tag*)))
    (make-stat :code (list ret)
	       :break-statements (list ret))))

(defun rewrite-break-statements (sym stat)
  (loop for i in (stat-break-statements stat)
     do (setf (second i) sym))
  (setf (stat-break-statements stat) nil))

(defvar *unresolved-continue-tag* (gensym "unresolved continue"))

(defun make-stat-unresolved-continue ()
  (let ((ret (list 'go *unresolved-continue-tag*)))
    (make-stat :code (list ret)
	       :continue-statements (list ret))))

(defun rewrite-continue-statements (sym stat)
  (loop for i in (stat-continue-statements stat)
     do (setf (second i) sym))
  (setf (stat-continue-statements stat) nil))

(defun extract-loop (body-stat
		     &key (init nil) (cond t) (step nil)
		     (post-test-p nil))
  (let ((loop-body-tag (gensym "(loop body)"))
	(loop-step-tag (gensym "(loop step)"))
	(loop-cond-tag (gensym "(loop cond)"))
	(loop-end-tag (gensym "(loop end)")))
    (rewrite-break-statements loop-end-tag body-stat)
    (rewrite-continue-statements loop-step-tag body-stat)
    (setf (stat-code body-stat)
	  `((progn ,init)
	    ,(if post-test-p
		 `(go ,loop-body-tag)		; do-while
		 `(go ,loop-cond-tag))
	    ,loop-body-tag
	    ,@(stat-code body-stat)
	    ,loop-step-tag
	    (progn ,step)
	    ,loop-cond-tag
	    (when (progn ,cond)
	      (go ,loop-body-tag))
	    ,loop-end-tag))
    body-stat))

(defun push-case-label (case-label-exp stat)
  (let ((go-tag-sym (gensym (format nil "(case ~S)" case-label-exp))))
    (setf (stat-case-label-list stat)
	  (acons go-tag-sym case-label-exp
		 (stat-case-label-list stat)))
    (push go-tag-sym (stat-code stat))))

(defun extract-switch (exp stat)
  (let* ((exp-sym (gensym "(switch cond)"))
	 (end-tag (gensym "(switch end)"))
	 (jump-table			; create jump table with COND
	  (loop with default-clause =`(t (go ,end-tag))
	     for (go-tag-sym . case-label-exp)
	     in (stat-case-label-list stat)

	     if (eq case-label-exp 'default)
	     do (setf default-clause `(t (go ,go-tag-sym)))
	     else
	     collect `((eql ,exp-sym ,case-label-exp) (go ,go-tag-sym))
	     into clauses
	     finally
               (return
                 `(let ((,exp-sym ,exp))
                    (cond
                      ,@clauses
                      ,default-clause))))))
    (rewrite-break-statements end-tag stat)
    (setf (stat-case-label-list stat) nil)
    (setf (stat-code stat)
	  `(,jump-table
	    ,@(stat-code stat)
	    ,end-tag))
    stat))

;;; Toplevel
;; returns (values auto-binds register-binds static-binds
;;                 extern-binds global-binds enum-const-binds
;;                 dynamic-established-syms classdefs)
(defun expand-decl-bindings (declaration-list default-storage-class)
  (loop with dynamic-established-syms = nil
     for (dspecs init-decls) in declaration-list
     as storage-class = (decl-specs-storage-class dspecs)
     as (var-binds func-binds)
       = (loop for i in init-decls
            as name = (init-declarator-lisp-name i)
	    as b = (list name (init-declarator-lisp-initform i))
	    if (subtypep (init-declarator-lisp-type i) 'function)
	    collect b into func-binds
	    else
	    collect b into var-binds
            and do (when (member name *dynamic-binding-requested*)
                     (push name dynamic-established-syms))
	    finally (return (list var-binds func-binds)))
     if (or (eq storage-class 'auto)
            (and (null storage-class) (eq default-storage-class 'auto)))
       nconc var-binds into auto-binds
     if (eq storage-class 'register)
       nconc var-binds into register-binds
     if (eq storage-class 'extern)
       nconc var-binds into extern-binds
     if (and (null storage-class) (eq default-storage-class 'global))
       nconc var-binds into global-binds
     if (eq storage-class 'static)
       nconc var-binds into static-binds

     append func-binds into extern-binds

     append (decl-specs-lisp-bindings dspecs) into enum-const-binds
     append (decl-specs-lisp-class-spec dspecs) into classes
     finally
       (return (values auto-binds register-binds extern-binds
                       global-binds static-binds enum-const-binds
                       dynamic-established-syms classes))))

(defun expand-class-spec (classes)
  (loop for wcsspec in classes
     as sname = (w-c-s-struct-spec-struct-name wcsspec)
     as iname = (w-c-s-struct-spec-internal-name wcsspec)
     as cls-slots = 
       (loop for slot-def in (w-c-s-struct-spec-slot-defs wcsspec)
          for index from 0
          collect
            (destructuring-bind (&key name lisp-type constness initform
                                      &allow-other-keys)
                slot-def
              `(,(gensym (format nil "field ~S " name))
                 :initarg ,(w-c-s-struct-initarg-symbol index)
                 ,(if constness :reader :accessor) ,name
                 ,@(if initform `(:initform ,initform) nil)
                 :type ,lisp-type)))
     collect `(defclass ,iname () ,cls-slots) into codes
     collect `(defclass ,sname (,iname) ()) into renames
     finally (return (values codes renames))))

;; mode is :statement or :translation-unit
(defun expand-toplevel (mode decls code additinal-binding)
    (multiple-value-bind (autos registers externs globals statics enum-consts
                                dynamic-established-syms cls)
        (expand-decl-bindings decls
                              (ecase mode
                                (:statement 'auto) (:translation-unit 'global)))
      (let* ((lexical-binds
              (append autos registers additinal-binding))
	     (register-vars (mapcar #'first registers))
             (bad-pointers (intersection dynamic-established-syms register-vars))
             (special-vars nil)
             (global-defs nil)
             (sym-macros nil))
        ;; 'auto' and 'register'
        (when (and (eq mode :translation-unit)
                   (or autos registers))
          (error "At top level, 'auto' or 'register' variables are not accepted (~S)"
                 (append autos registers)))
        ;; 'register' vars
        (when bad-pointers
          (warn "some variables are 'register', but its pointer is taken (~S)."
                bad-pointers))
        ;; 'extern' vars.
        (loop for (var init) in externs
           unless (or (null init) (zerop init))
           do (error "an 'extern' variable cannot have initializer (~S = ~S)" var init))
        ;; 'global' vars.
        (when (and (eq mode :statement)
                   globals)
          (error "In internal scope, no global vars cannot be defined (~S)." globals))
        (loop for (var init) in globals
           do (push var special-vars)
           do (push `(defvar ,var ,init "generated by with-c-syntax, for global") global-defs))
        ;; 'static' vars.
        (loop for (var init) in statics
           as st-sym = (gensym (format nil "symbol-for-static-var ~S" var))
           do (push st-sym special-vars)
           do (push `(defvar ,st-sym ,init "generated by with-c-syntax, for static") global-defs)
           do (push `(,var ,st-sym) sym-macros))
        ;; enum consts
        (nconcf sym-macros enum-consts)
        ;; structs
        (multiple-value-bind (codes renames)
            (expand-class-spec cls)
          (nconcf global-defs codes)
          (when (eq mode :translation-unit)
            (nconcf global-defs renames)))
        (prog1
            `(symbol-macrolet ,sym-macros
               (declare (special ,@special-vars))
               ,@global-defs
               (let* ,lexical-binds
                 (declare (dynamic-extent ,@register-vars))
                 ;; If no pointers requied, remove dynamic binds.
                 ;; This makes the compiler faster.
                 ,(if *dynamic-binding-requested*
                      `(with-dynamic-bound-symbols ,*dynamic-binding-requested*
                         (block nil ,@code))
                      `(block nil ,@code))))
          ;; drop dynamic-binds
          (loop for sym in dynamic-established-syms
             do (setf *dynamic-binding-requested*
                      (delete sym *dynamic-binding-requested*
                              :test #'eq :count 1)))
          ;; drop enum-binds
          (loop for (sym _) in enum-consts
             do (setf *enum-const-symbols*
                      (delete sym *enum-const-symbols*
                              :test #'eq :count 1)))
          ;; drop structure-binds
          (loop for c in cls
             do (drop-w-c-s-struct-spec c))
          ))))

(defun expand-toplevel-stat (stat &key entry-point)
  (expand-toplevel :statement
                   (stat-declarations stat)
                   `((tagbody ,@(stat-code stat)))
                   (if entry-point *toplevel-bindings* nil)))

(defstruct function-definition
  lisp-code
  lisp-type)

(defun lispify-function-definition (name body
                                    &key (return *default-decl-specs*)
                                      (K&R-decls nil))
  (let* ((func-name (first name))
         (func-param (getf (second name) :funcall))
         (param-ids
          (loop for (dspec tspecs) in func-param
             collect (first tspecs))))
    (setf return (finalize-decl-specs return))
    (when K&R-decls
      (let* ((K&R-param-ids
              (mapcar #'first (expand-decl-bindings K&R-decls 'auto))))
        (unless (equal K&R-param-ids param-ids)
          (error "prototype is not matched with k&r-style params"))))
    (make-function-definition
     :lisp-code `((defun ,func-name ,param-ids
                    ,(expand-toplevel-stat body)))
     :lisp-type `(function ',(mapcar (constantly t) param-ids)
                           ',(decl-specs-lisp-type return)))))

(defun expand-translation-unit (units)
  (loop
     for u in units
     if (function-definition-p u)
     append (function-definition-lisp-code u) into codes
     else
     collect u into decls
     finally
       (return (expand-toplevel :translation-unit
                                decls codes *toplevel-bindings*))))

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
   (translation-unit
    #'(lambda (us) (expand-translation-unit us)))
   (labeled-stat
    #'(lambda (st) (expand-toplevel-stat st :entry-point t)))
   ;; exp-stat is not included, because it is gramatically ambiguous.
   (compound-stat
    #'(lambda (st) (expand-toplevel-stat st :entry-point t)))
   (selection-stat
    #'(lambda (st) (expand-toplevel-stat st :entry-point t)))
   (iteration-stat
    #'(lambda (st) (expand-toplevel-stat st :entry-point t)))
   (jump-stat
    #'(lambda (st) (expand-toplevel-stat st :entry-point t))))


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
	(lispify-function-definition name body
				     :return ret
				     :K&R-decls k&r-decls)))
   (           declarator decl-list compound-stat
    #'(lambda (name k&r-decls body)
	(lispify-function-definition name body
				     :K&R-decls k&r-decls)))
   (decl-specs declarator           compound-stat
    #'(lambda (ret name body)
	(lispify-function-definition name body
				     :return ret)))
   (           declarator           compound-stat
    #'(lambda (name body)
	(lispify-function-definition name body))))

  (decl
   (decl-specs init-declarator-list \;
               #'(lambda (dcls inits _t)
                   (declare (ignore _t))
		   (setf dcls (finalize-decl-specs dcls))
		   `(,dcls
		     ,(mapcar #'(lambda (i) (finalize-init-declarator dcls i))
			      inits))))
   (decl-specs \;
               #'(lambda (dcls _t)
                   (declare (ignore _t))
		   (setf dcls (finalize-decl-specs dcls))
		   `(,dcls))))

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
   struct-or-union-spec
   enum-spec
   typedef-name)                        ; not supported -- TODO!!

  (type-qualifier
   const volatile)                      ; keywords

  ;; returns struct-or-union-spec structure
  (struct-or-union-spec
   (struct-or-union id { struct-decl-list }
                    #'(lambda (kwd id _l decl _r)
                        (declare (ignore _l _r))
			(make-struct-or-union-spec
			 :type kwd :id id :struct-decl-list decl)))
   (struct-or-union    { struct-decl-list }
                    #'(lambda (kwd _l decl _r)
                        (declare (ignore _l _r))
			(make-struct-or-union-spec
			 :type kwd :struct-decl-list decl)))
   (struct-or-union id
                    #'(lambda (kwd id)
			(make-struct-or-union-spec
			 :type kwd :id id))))

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
	(make-struct-declarator :declarator d)))
   (declarator \: const-exp
	       #'(lambda (d _c bits)
		   (declare (ignore _c))
		   (make-struct-declarator :declarator d :bits bits)))
   (\: const-exp
       #'(lambda (_c bits)
	   (declare (ignore _c))
	   (make-struct-declarator :bits bits))))

  ;; returns enum-spec structure
  (enum-spec
   (enum id { enumerator-list }
         #'(lambda (_kwd id _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :id id :enumerator-list lis)))
   (enum    { enumerator-list }
         #'(lambda (_kwd _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :enumerator-list lis)))
   (enum id
         #'(lambda (_kwd id)
             (declare (ignore _kwd))
	     (make-enum-spec :id id))))

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

  (declarator
   (pointer direct-declarator
    #'(lambda (ptr dcls)
        (append dcls ptr)))
   direct-declarator)

  (direct-declarator
   (id
    #'list)
   (\( declarator \)
    #'(lambda (_lp dcl _rp)
	(declare (ignore _lp _rp))
        dcl))
   (direct-declarator [ const-exp ]
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
        `(,@dcl (:aref ,params))))
   (direct-declarator [		  ]
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
        `(,@dcl (:aref nil))))
   (direct-declarator \( param-type-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
        `(,@dcl (:funcall ,params))))
   (direct-declarator \( id-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
        `(,@dcl (:funcall
                 ;; make as a list of (decl-spec (id))
                 ,(mapcar #'(lambda (p) `(nil (,p))) params)))))
   (direct-declarator \(	 \)
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
        `(,@dcl (:funcall nil)))))

  (pointer
   (* type-qualifier-list
    #'(lambda (_kwd qls)
        (declare (ignore _kwd))
        `((:pointer ,@qls))))
   (*
    #'(lambda (_kwd)
        (declare (ignore _kwd))
        `((:pointer))))
   (* type-qualifier-list pointer
    #'(lambda (_kwd qls ptr)
        (declare (ignore _kwd))
        `(,@ptr (:pointer ,@qls))))
   (*			  pointer
    #'(lambda (_kwd ptr)
        (declare (ignore _kwd))
        `(,@ptr (:pointer)))))
			  

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

  (param-decl
   (decl-specs declarator
	       #'list)
   (decl-specs abstract-declarator
	       #'list)
   (decl-specs
    #'list))

  (id-list
   (id
    #'list)
   (id-list \, id
    #'concatinate-comma-list))

  ;; returns a struct, if initializer-list is used.
  (initializer
   assignment-exp
   ({ initializer-list }
    #'(lambda (_lp inits _rp)
	(declare (ignore _lp _rp))
        inits))
   ({ initializer-list \, }
    #'(lambda (_lp inits _cm _rp)
	(declare (ignore _lp _cm _rp))
        inits)))

  (initializer-list
   (initializer
    #'list)
   (initializer-list \, initializer
    #'concatinate-comma-list))

  ;; see 'decl'
  (type-name
   (spec-qualifier-list abstract-declarator
			#'(lambda (qls abs)
			    (lispify-type-name qls abs)))
   (spec-qualifier-list
    #'(lambda (qls)
	(lispify-type-name qls nil))))

  ;; inserts 'nil' as a name
  (abstract-declarator
   (pointer
    #'(lambda (ptr)
	`(nil ,@ptr)))
   (pointer direct-abstract-declarator
    #'(lambda (ptr dcls)
	`(nil ,@dcls ,@ptr)))
   (direct-abstract-declarator
    #'(lambda (adecl)
	`(nil ,@adecl))))

  ;; returns like:
  ;; (:aref nil) (:funcall nil) (:aref 5 :funcall (int))
  (direct-abstract-declarator
   (\( abstract-declarator \)
    #'(lambda (_lp dcl _rp)
	(declare (ignore _lp _rp))
        dcl))
   (direct-abstract-declarator [ const-exp ]
    #'(lambda (dcls _lp params _rp)
	(declare (ignore _lp _rp))
        `(,@dcls (:aref ,params))))
   (			       [ const-exp ]
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
        `((:aref ,params))))
   (direct-abstract-declarator [	   ]
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
        `(,@dcls (:aref nil))))
   (			       [	   ]
    #'(lambda (_lp _rp)
	(declare (ignore _lp _rp))
        '((:aref nil))))
   (direct-abstract-declarator \( param-type-list \)
    #'(lambda (dcls _lp params _rp)
	(declare (ignore _lp _rp))
        `(,@dcls (:funcall ,params))))
   (			       \( param-type-list \)
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
        `((:funcall ,params))))
   (direct-abstract-declarator \(		  \)
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
        `(,@dcls (:funcall nil))))
   (			       \(		  \)
    #'(lambda (_lp _rp)
	(declare (ignore _lp _rp))
        '((:funcall nil)))))


  ;; ;; TODO
  ;; (typedef-name
  ;;  id)


  ;;; Statements: 'stat' structure
  ;; 
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
	   (push id (stat-code stat))
	   stat))
   (case const-exp \: stat
       #'(lambda (_k  exp _c stat)
	   (declare (ignore _k _c))
	   (push-case-label exp stat)
	   stat))
   (default \: stat
       #'(lambda (_k _c stat)
	   (declare (ignore _k _c))
	   (push-case-label 'default stat)
	   stat)))

  (exp-stat
   (exp \;
	#'(lambda (exp _term)
	    (declare (ignore _term))
	    (make-stat :code (list exp))))
   (\;
    #'(lambda (_term)
	(declare (ignore _term))
	(make-stat))))

  (compound-stat
   ({ decl-list stat-list }
      #'(lambda (_op1 dcls stat _op2)
          (declare (ignore _op1 _op2))
	  (setf (stat-declarations stat)
		(append dcls (stat-declarations stat)))
	  stat))
   ({ stat-list }
      #'(lambda (op1 stat op2)
	  (declare (ignore op1 op2))
	  stat))
   ({ decl-list	}
      #'(lambda (_op1 dcls _op2)
	  (declare (ignore _op1 _op2))
	  (make-stat :declarations dcls)))
   ({ }
      #'(lambda (op1 op2)
	  (declare (ignore op1 op2))
	  (make-stat))))

  (stat-list
   stat
   (stat-list stat
    #'(lambda (st1 st2)
	(merge-stat st1 st2 :merge-code t))))

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
	     (make-stat :code (list `(go ,id)))))
   (continue \;
	     #'(lambda (_k _t)
		 (declare (ignore _k _t))
		 (make-stat-unresolved-continue)))
   (break \;
	  #'(lambda (_k _t)
	      (declare (ignore _k _t))
	      (make-stat-unresolved-break)))
   (return exp \;
	   #'(lambda (_k exp _t)
	       (declare (ignore _k _t))
	       ;; use the block of PROG
	       (make-stat :code (list `(return ,exp)))))
   (return \;
	   #'(lambda (_k _t)
	       (declare (ignore _k _t))
	       ;; use the block of PROG
	       (make-stat :code (list `(return (values)))))))


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
              (progn
                (push exp *dynamic-binding-requested*)
                `(make-pseudo-pointer* ,exp ',exp))
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
   (sizeof unary-exp			; TODO: add struct
	   #'(lambda (_op exp)
	       (declare (ignore _op))
	       `(if (arrayp ,exp)
		    (array-total-size ,exp)
		    1)))
   (sizeof \( type-name \)		; TODO: add struct
	   #'(lambda (_op _lp tp _rp)
	       (declare (ignore _op _lp _rp))
	       (if (subtypep tp 'array)
		   (array-total-size (make-array (third tp)))
		   1))))

  (postfix-exp
   primary-exp
   (postfix-exp [ exp ]			; TODO: compound with multi-dimention
		#'(lambda (exp op1 idx op2)
		    (declare (ignore op1 op2))
                    (if (and (listp exp) (eq (first exp) 'aref))
                        (destructuring-bind (op exp &rest args) exp
                          `(,op ,exp ,@args ,idx))
                        `(aref ,exp ,idx))))
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
(defun c-expression-tranform (bindings form)
  (with-new-w-c-s-environment ()
    (let ((lisp-exp (parse-with-lexer (list-lexer form)
                                      *expression-parser*)))
      lisp-exp)))

;;; Macro interface
(defmacro with-c-syntax ((&rest bindings) &body body)
  (c-expression-tranform bindings body))
