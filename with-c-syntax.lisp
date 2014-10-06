(in-package :with-c-syntax)

;;; Constants
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +operators+
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
	++ -- |sizeof|
	& * + - ~ !
	[ ] \. ->
	)
    :test 'equal)

  (define-constant +keywords+
      '(\;
	|auto| |register| |static| |extern| |typedef|
	|void| |char| |short| |int| |long|
        |float| |double| |signed| |unsigned|
	|const| |volatile|
	|struct| |union|
	|enum|
	|...|
	|case| |default|
	{ }
	|if| |else| |switch|
	|while| |do| |for|
	|goto| |continue| |break| |return|
	)
    :test 'equal))

(define-constant +predefined-typedef-names+
    '((|size_t| fixnum) (|wchar_t| fixnum) (|va_list| t))
  :test 'equal)

;;; Variables
(defvar *enum-const-symbols* nil
  "list of (symbol initform)")
(defvar *dynamic-binding-requested* nil)
(defvar *wcs-struct-specs* (make-hash-table :test 'eq)
  "hashtable: struct-name -> list of wcs-struct-spec.
If a same name is supplied, it is stacked")
(defvar *typedef-names* (make-hash-table :test 'eq)
  "hashtable: symbol -> list of decl-specs")
(defvar *function-pointer-ids* nil
  "list of symbol")

(defmacro with-new-wcs-environment (() &body body)
  `(let* ((*enum-const-symbols* nil)
          (*dynamic-binding-requested* nil)
          (*wcs-struct-specs* (make-hash-table :test 'eq))
          (*typedef-names* (make-hash-table :test 'eq))
          (*function-pointer-ids* nil))
     ,@body))

;;; Lexer
(defun list-lexer (list symbol-case)
  (let ((keyword-key-fn
         (if (eq symbol-case :upcase)
             #'string-upcase #'identity)))
    #'(lambda ()
        (let ((value (pop list)))
          (typecase value
            (null
             (values nil nil))
            (symbol
             (if-let
                 ((op (or (member value +operators+
                                  :key keyword-key-fn
                                  :test #'string=)
                          (member value +keywords+
                                  :key keyword-key-fn
                                  :test #'string=))))
               (values (car op) (car op)) ; returns the symbol of our package.
               (if-let
                   ((ptypedef (member value +predefined-typedef-names+
                                       :key (compose keyword-key-fn #'car)
                                       :test #'string=)))
                 (values 'typedef-id (caar ptypedef))
                 (cond ((member value *enum-const-symbols*)
                        (values 'enumeration-const value))
                       ((gethash value *typedef-names*)
                        (values 'typedef-id value))
                       (t
                        (values 'id value))))))
            (integer
             (values 'int-const value))
            (character
             (values 'char-const value))
            (float
             (values 'float-const value))
            (string
             (values 'string value))
            (list
             (values 'lisp-expression value))
            (t
             (error "Unexpected value ~S" value)))))))

;;; Declarations
(defstruct decl-specs
  ;; Filled by the parser
  (type-spec nil)
  (storage-class nil)
  (qualifier nil)
  ;; Filled by 'finalize-decl-specs'
  (lisp-type t)             ; typename for Common Lisp
  (wcs-type-tag nil)        ; user supplied struct name (or enum name)
  (lisp-bindings nil)       ; enum
  (lisp-class-spec nil)     ; wcs-struct-spec
  (typedef-init-decl nil))  ; typedef

(defstruct init-declarator
  ;; Filled by the parser
  declarator
  (initializer nil)
  ;; Filled by 'finalize-init-declarator'
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

;; typedefs
(defparameter *default-decl-specs*
  (make-decl-specs))
(defparameter *default-decl-specs-fixnum*
  (make-decl-specs :lisp-type 'fixnum))

(defun push-typedef-name (name dspecs)
  (push dspecs (gethash name *typedef-names*)))

(defun find-typedef-name (name)
  (if-let ((udef (gethash name *typedef-names*)))
    (first udef)
    (if-let ((pdef (member name +predefined-typedef-names+
			   :key #'first
			   :test #'string=)))
      (ecase (second (car pdef))
	(fixnum *default-decl-specs-fixnum*)
	((t) *default-decl-specs*))
      (if-let ((sdef (member name +standardized-atomic-type-specifiers+
			     :test #'eq)))
	(make-decl-specs :lisp-type (car sdef))
	nil))))

(defun drop-typedef-name (name)
  (pop (gethash name *typedef-names*)))

;; structure information
(defstruct wcs-struct-spec
  struct-name                           ; user supplied name (symbol)
  internal-name                         ; internal name (gensym)
  struct-type                           ; 'struct or 'union
  ;; (:lisp-type ... :constness ... :decl-specs ...)
  slot-defs
  runtime-spec)

(defun ensure-wcs-struct-spec (name)
  (let ((wcsspec (first (gethash name *wcs-struct-specs*))))
    (unless wcsspec
      (setf wcsspec
            (make-wcs-struct-spec
             :struct-name name
             :internal-name (gensym (format nil "struct ~S " name))))
      (push wcsspec (gethash name *wcs-struct-specs*)))
    wcsspec))

(defun find-wcs-struct-spec (name)
  (if-let ((wcsspec (first (gethash name *wcs-struct-specs*))))
    wcsspec
    (error "struct ~S is not defined" name)))

(defun drop-wcs-struct-spec (wcsspec)
  (let ((name (wcs-struct-spec-struct-name wcsspec)))
    (symbol-macrolet ((place (gethash name *wcs-struct-specs*)))
      (removef place wcsspec :key #'wcs-struct-spec-internal-name))))

(defun wcs-struct-spec-fill-runtime-spec (wcsspec)
  (let* ((stype (wcs-struct-spec-struct-type wcsspec))
         (runtime-spec
          (loop for slot-def in (wcs-struct-spec-slot-defs wcsspec)
             for index from 0
             collect (getf slot-def :name) into names
	     collect (getf slot-def :initform) into initforms
	     finally
	       (return
		 (make-wcs-struct-runtime-spec names (eq stype '|union|)
					        initforms)))))
    (setf (wcs-struct-spec-runtime-spec wcsspec)
          runtime-spec))
  wcsspec)

;; processes structure-spec 
(defun finalize-struct-spec (sspec dspecs)
  (let* ((wcsname (or (struct-or-union-spec-id sspec)
                      (gensym "unnamed-struct-")))
         (wcsspec (ensure-wcs-struct-spec wcsname)))
    (setf (decl-specs-wcs-type-tag dspecs) wcsname)
    (setf (decl-specs-lisp-type dspecs) 'wcs-struct)
    ;; only declaration?
    (when (null (struct-or-union-spec-struct-decl-list sspec))
      (assert (struct-or-union-spec-id sspec)) ; this case is rejected by the parser.
      (return-from finalize-struct-spec dspecs))
    ;; Doubly defined?
    (when (and (struct-or-union-spec-struct-decl-list sspec)
               (wcs-struct-spec-slot-defs wcsspec))
      (error "struct is defined twice (~S)" wcsname))
    ;; Now defines a new struct.
    (setf (wcs-struct-spec-struct-type wcsspec)
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
            with constness = (member '|const| (decl-specs-qualifier spec-qual))
            for s-decl in struct-decls
            as (decl-name . abst-decl) = (init-declarator-declarator s-decl)
            as name = (or decl-name (gensym "unnamed-field-"))
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
         (setf (wcs-struct-spec-slot-defs wcsspec) slot-specs))
    ;; This wcsspec is treated by this dspecs
    (append-item-to-right-f (decl-specs-lisp-class-spec dspecs)
                            wcsspec)
    dspecs))

;; processes enum-spec 
(deftype wcs-enum ()
  'fixnum)

(defun finalize-enum-spec (espec dspecs)
  (setf (decl-specs-lisp-type dspecs) 'wcs-enum)
  (setf (decl-specs-wcs-type-tag dspecs)
	(or (enum-spec-id espec) (gensym "unnamed-enum-")))
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

     initially
       (when (null tp-list)
         (setf (decl-specs-lisp-type dspecs)
	       (decl-specs-lisp-type *default-decl-specs*))
         (return dspecs))
       
     for tp-list-1 on tp-list
     for tp = (car tp-list-1)

     if (eq tp '|void|)			; void
     do (unless (length= 1 tp-list)
	  (error "invalid decl-spec (~A)" tp-list))
       (setf (decl-specs-lisp-type dspecs) nil)
     and return dspecs

     else if (struct-or-union-spec-p tp) ; struct / union
     do (unless (length= 1 tp-list)
	  (error "invalid decl-spec (~A)" tp-list))
     and return (finalize-struct-spec tp dspecs)

     else if (enum-spec-p tp)	; enum
     do (unless (length= 1 tp-list)
	  (error "invalid decl-spec (~A)" tp-list))
     and return (finalize-enum-spec tp dspecs)

     ;; numeric types
     else if (member tp '(|float| |double| |int| |char|))
     do (when numeric-type
	  (error "invalid decl-spec (~A)" tp-list))
       (setf numeric-type tp)
     ;; numeric variants
     else if (member tp '(|signed| |unsigned|))
     do (when numeric-signedness
	  (error "invalid decl-spec (~A)" tp-list))
       (setf numeric-signedness tp)
     else if (eq tp '|long|)
     do (unless (<= 0 numeric-length 1)
	  (error "invalid decl-spec (~A)" tp-list))
       (incf numeric-length)
     else if (eq tp '|short|)
     do (unless (= 0 numeric-length)
	  (error "invalid decl-spec (~A)" tp-list))
       (decf numeric-length)

     else if (find-typedef-name tp)     ; typedef name
     do (let* ((td-dspecs (find-typedef-name tp))
               (td-dspecs-tp (decl-specs-lisp-type td-dspecs)))
          (case td-dspecs-tp
            ;; non-numeric
            ((nil wcs-struct wcs-enum t)
             (unless (length= 1 tp-list)
               (error "invalid decl-spec (~A)" tp-list))
             (setf (decl-specs-lisp-type dspecs)
                   (decl-specs-lisp-type td-dspecs)
                   (decl-specs-wcs-type-tag dspecs)
                   (decl-specs-wcs-type-tag td-dspecs)
                   ;; TODO: ??
                   (decl-specs-typedef-init-decl dspecs)
                   (decl-specs-typedef-init-decl td-dspecs))
             (return dspecs))
            ;; numeric. merge its contents to the parental one.
            (short-float (appendf tp-list-1 '(|short| |float|)))
            (single-float (appendf tp-list-1 '(|float|)))
            (double-float (appendf tp-list-1 '(|double|)))
            (long-float (appendf tp-list-1 '(|long| |double|)))
            (fixnum (appendf tp-list-1 '(|int|)))
            (t (destructuring-bind (byte-type bits) td-dspecs-tp
                 (when (eq 'unsigned-byte byte-type)
                   (appendf tp-list-1 '(|unsigned|)))
                 (ecase bits
                   (8 (appendf tp-list-1 '(|char|)))
                   (16 (appendf tp-list-1 '(|short|)))
                   (32 (appendf tp-list-1 '(|long|)))
                   (64 (appendf tp-list-1 '(|long| |long|))))))))
     else
     do (assert nil)
       
     finally
       (setf (decl-specs-lisp-type dspecs)
	     (ecase numeric-type
	       (|float|
		(when (or numeric-signedness
			  (not (member numeric-length '(-1 0))))
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq numeric-length -1)
		    'short-float 'single-float))
	       (|double|
		(when (or numeric-signedness
			  (not (member numeric-length '(0 1))))
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq numeric-length 1)
		    'long-float 'double-float))
	       (|char|
		(unless (zerop numeric-length)
		  (error "invalid decl-spec (~A)" tp-list))
		(if (eq '|unsigned| numeric-signedness) ; raw 'char' is signed
		    '(unsigned-byte 8)
		    '(signed-byte 8)))
	       ((|int| nil)
		(if (eq '|unsigned| numeric-signedness)
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
     for a-elem in array-dimension-list
     for (i-elem . i-elem-rest) = init-dims then i-elem-rest
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
(defun expand-init-declarator-init (dspecs abst-declarator initializer
                                    &key (error-incompleted-array t))
  (ecase (car (first abst-declarator))
    (:pointer
     (let ((next-type
	    (nth-value 1 (expand-init-declarator-init
			  dspecs (cdr abst-declarator) nil
			  :error-incompleted-array nil))))
       (values (or initializer 0)
               `(pseudo-pointer ,next-type))))
    (:funcall
     (when (eq :aref (car (second abst-declarator)))
       (error "a function returning an array is not accepted"))
     (when (eq :funcall (car (second abst-declarator)))
       (error "a function returning a function is not accepted"))
     (when initializer
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
                do (setf aref-type `(pseudo-pointer ,aref-type))
                  (loop-finish)
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
                          (null initializer)
                          ;; This error is not needed when we concerns only the type.
                          error-incompleted-array)
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
       (cond
         ((null var-type)
          (error "a void variable cannot be initialized"))
         ((eq var-type 't)
          (values initializer var-type))
         ((subtypep var-type 'number) ; includes enum
          (values (or initializer 0) var-type))
         ((subtypep var-type 'wcs-struct)
          (let* ((wcs-tag (decl-specs-wcs-type-tag dspecs))
                  ;; This is required, for treating a struct defined after declarations.
                 (wcsspec (find-wcs-struct-spec wcs-tag))
                 (init-list
                  (loop for idx from 0
                     for i in initializer
                     for slot in (wcs-struct-spec-slot-defs wcsspec)
                     collect (expand-init-declarator-init
                              (getf slot :decl-specs) (cdr abst-declarator) i))))
            (wcs-struct-spec-fill-runtime-spec wcsspec)
            (values `(make-wcs-struct
                      ;; internal-name is bound to the runtime-spec at top-level expansion.
                      ,(wcs-struct-spec-internal-name wcsspec)
                      ,@init-list)
                    var-type)))
         (t (error "Internal error: unknown type ~S" var-type)))))))

(defun finalize-init-declarator (dspecs init-decl)
  (let* ((decl (init-declarator-declarator init-decl))
         (init (init-declarator-initializer init-decl))
         (var-name (first decl))
         (abst-decl (rest decl))
	 (storage-class (decl-specs-storage-class dspecs)))
    (when (and init
               (member storage-class '(|extern| |typedef|)))
      (error "This variable (~S) cannot have any initializers" storage-class))
    (when (and (eq :funcall (car (second decl)))
	       (not (member storage-class '(nil |extern|))))
      (error "a function cannot have storage-class except 'extern'"))
    (when-let (td-init-decl (decl-specs-typedef-init-decl dspecs))
      (appendf abst-decl
               (cdr (init-declarator-declarator td-init-decl))))
    (multiple-value-bind (var-init var-type)
	(expand-init-declarator-init dspecs abst-decl init)
      (setf (init-declarator-lisp-name init-decl) var-name
	    (init-declarator-lisp-initform init-decl) var-init
	    (init-declarator-lisp-type init-decl) var-type)
      (when (and (listp var-type)
                 (eq 'pseudo-pointer (first var-type))
                 (subtypep (second var-type) 'function))
        (push var-name *function-pointer-ids*)))
    (when (eq '|typedef| storage-class)
      (setf (decl-specs-typedef-init-decl dspecs) init-decl)
      (push-typedef-name var-name dspecs))
    init-decl))

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
)

(defun lispify-type-name (qls abs)
  (setf qls (finalize-decl-specs qls))
  (if abs
      (let ((init-decl (make-init-declarator :declarator abs)))
	(setf init-decl (finalize-init-declarator qls init-decl))
        (init-declarator-lisp-type init-decl))
      (decl-specs-lisp-type qls)))

;;; Pointers
(defun wcs-aref (obj arg1 &rest args)
  (etypecase obj
    (pseudo-pointer
     (if (null args)
	 (pseudo-pointer-dereference (+ obj arg1))
	 (apply #'wcs-aref
		(pseudo-pointer-dereference (+ obj arg1))
		args)))
    (array
     (apply #'aref obj arg1 args))))

(defun (setf wcs-aref) (val obj arg1 &rest args)
  (etypecase obj
    (pseudo-pointer
     (if (null args)
	 (setf (pseudo-pointer-dereference (+ obj arg1)) val)
	 (setf (apply #'wcs-aref
		      (pseudo-pointer-dereference (+ obj arg1))
		      args)
	       val)))
    (array
     (setf (apply #'aref obj arg1 args) val))))

(defun lispify-address-of (exp)
  (cond ((symbolp exp)
	 (push exp *dynamic-binding-requested*)
	 (let ((val (gensym)))
	   `(let ((,val ,exp))
	      (make-pseudo-pointer
	       (if (pseudo-pointer-pointable-p ,val)
		   ,val ',exp)))))
        ((listp exp)
	 (destructuring-ecase exp
	   ((wcs-aref obj &rest args)
	    (once-only (obj)
              `(if (arrayp ,obj)
		   (make-pseudo-pointer
		    (make-reduced-dimension-array ,obj ,@(butlast args))
		    ,(lastcar args))
                   (error "Getting a pointer to an array, but this is not an array: ~S"
                          ,obj))))
           ((wcs-struct-field obj field)
	    (once-only (obj)
              `(if (typep ,obj 'wcs-struct)
		   (make-pseudo-pointer
		    (wcs-struct-fields ,obj)
		    (wcs-struct-field-index ,obj ,field))
                   (error "Getting a pointer to a struct member, but this is not a struct: ~S"
                          ,obj))))
           ((pseudo-pointer-dereference obj)
            obj)))
	(t
	 (error "cannot take a pointer to form ~S" exp))))

(defun lispify-funcall (func-exp args)
  (if (and (symbolp func-exp)
           (not (member func-exp *function-pointer-ids*)))
      `(,func-exp ,@args)
      `(funcall ,func-exp ,@args)))

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
	 (then-tag (gensym "if-then-"))
	 (else-tag (gensym "if-else-"))
	 (end-tag (gensym "if-end-")))
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

(defvar *unresolved-break-tag* (gensym "unresolved-break-"))

(defun make-stat-unresolved-break ()
  (let ((ret (list 'go *unresolved-break-tag*)))
    (make-stat :code (list ret)
	       :break-statements (list ret))))

(defun rewrite-break-statements (sym stat)
  (loop for i in (shiftf (stat-break-statements stat) nil)
     do (setf (second i) sym)))

(defvar *unresolved-continue-tag* (gensym "unresolved-continue-"))

(defun make-stat-unresolved-continue ()
  (let ((ret (list 'go *unresolved-continue-tag*)))
    (make-stat :code (list ret)
	       :continue-statements (list ret))))

(defun rewrite-continue-statements (sym stat)
  (loop for i in (shiftf (stat-continue-statements stat) nil)
     do (setf (second i) sym)))

(defun extract-loop (body-stat
		     &key (init nil) (cond t) (step nil)
		     (post-test-p nil))
  (let ((loop-body-tag (gensym "loop-body-"))
	(loop-step-tag (gensym "loop-step-"))
	(loop-cond-tag (gensym "loop-cond-"))
	(loop-end-tag (gensym "loop-end-")))
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
  (let ((go-tag-sym (gensym (format nil "case-~S-" case-label-exp))))
    (push (cons go-tag-sym case-label-exp)
          (stat-case-label-list stat))
    (push go-tag-sym (stat-code stat))))

(defun extract-switch (exp stat)
  (let* ((exp-sym (gensym "switch-cond-"))
	 (end-tag (gensym "switch-end-"))
	 (jump-table			; create jump table with COND
	  (loop with default-clause =`(t (go ,end-tag))
	     for (go-tag-sym . case-label-exp)
	     in (shiftf (stat-case-label-list stat) nil)

	     if (eq case-label-exp '|default|)
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
     with funcptr-syms = nil
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
            and do (when (member name *function-pointer-ids*)
                     (push name funcptr-syms))
	    finally (return (list var-binds func-binds)))
     if (or (eq storage-class '|auto|)
            (and (null storage-class) (eq default-storage-class '|auto|)))
       nconc var-binds into auto-binds
     if (eq storage-class '|register|)
       nconc var-binds into register-binds
     if (eq storage-class '|extern|)
       nconc var-binds into extern-binds
     if (and (null storage-class) (eq default-storage-class '|global|))
       nconc var-binds into global-binds
     if (eq storage-class '|static|)
       nconc var-binds into static-binds
     if (eq storage-class '|typedef|)
       nconc var-binds into typedef-binds

     append func-binds into extern-binds

     append (decl-specs-lisp-bindings dspecs) into enum-const-binds
     append (decl-specs-lisp-class-spec dspecs) into classes
     finally
       (return (values auto-binds register-binds extern-binds
                       global-binds static-binds typedef-binds
                       enum-const-binds classes
		       (nreverse dynamic-established-syms)
		       (nreverse funcptr-syms)))))

(defun expand-struct-spec (classes)
  (loop for wcsspec in classes
     as sname = (wcs-struct-spec-struct-name wcsspec)
     as iname = (wcs-struct-spec-internal-name wcsspec)
     as spec-form = (make-load-form (wcs-struct-spec-runtime-spec wcsspec))
     collect `(,iname ,spec-form) into class-binds
     collect `(,sname ,iname) into renames
     finally (return (values class-binds renames))))

;; mode is :statement or :translation-unit
(defun expand-toplevel (mode decls code)
    (multiple-value-bind (autos registers externs globals statics typedefs
                                enum-consts classes
				dynamic-established-syms funcptr-syms)
        (expand-decl-bindings decls
                              (ecase mode
                                (:statement '|auto|) (:translation-unit '|global|)))
      (let* ((lexical-binds
              (append autos registers))
	     (register-vars (mapcar #'first registers))
             (bad-pointers (intersection dynamic-established-syms register-vars))
             (special-vars nil)
             (global-defs nil)
             (sym-macros nil)
	     (struct-defs nil))
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
           as st-sym = (gensym (format nil "static-var-~S-" var))
           do (push st-sym special-vars)
           do (push `(defvar ,st-sym ,init "generated by with-c-syntax, for static") global-defs)
           do (push `(,var ,st-sym) sym-macros))
        ;; enum consts
        (nconcf sym-macros enum-consts)
        ;; structs
        (multiple-value-bind (class-binds renames)
            (expand-struct-spec classes)
	  (setf lexical-binds (nconc class-binds lexical-binds))
          (when (eq mode :translation-unit)
	    (loop for (sym val) in renames
	       do (push `(install-wcs-struct-runtime-spec ',sym ,val)
			struct-defs))))
        (prog1
            `(symbol-macrolet ,sym-macros
               (declare (special ,@special-vars))
               ,@global-defs
               (let* (,@lexical-binds)
                 (declare (dynamic-extent ,@register-vars))
		 ,@struct-defs
		 (with-dynamic-bound-symbols ,*dynamic-binding-requested*
		   (block nil ,@code))))
          ;; drop dynamic-binds
          (loop for sym in dynamic-established-syms
             do (deletef *dynamic-binding-requested*
			 sym :test #'eq :count 1))
          ;; drop typedefs
          (loop for (sym _) in typedefs
             do (drop-typedef-name sym))
          ;; drop enum-binds
          (loop for (sym _) in enum-consts
             do (deletef *enum-const-symbols*
			 sym :test #'eq :count 1))
          ;; drop structure-binds
          (loop for c in classes
             do (drop-wcs-struct-spec c))
	  ;; drop function-pointer-ids
          (loop for sym in funcptr-syms
	     do (deletef *function-pointer-ids*
			 sym :test #'eq :count 1))))))

(defun expand-toplevel-stat (stat)
  (expand-toplevel :statement
                   (stat-declarations stat)
                   `((tagbody ,@(stat-code stat)))))

(defstruct function-definition
  lisp-code
  lisp-type)

(defun lispify-function-definition (name body &key return K&R-decls)
  (let* ((func-name (first name))
         (func-param (getf (second name) :funcall))
         (variadic nil)
         (param-ids
          (loop for p in func-param
             if (eq p '|...|)
             do (setf variadic t) (loop-finish)
             else
             collect (first (second p)))))
    (setf return
	  (if return (finalize-decl-specs return)
	      *default-decl-specs*))
    (when K&R-decls
      (let ((K&R-param-ids
	     (mapcar #'first (expand-decl-bindings K&R-decls '|auto|))))
        (unless (equal K&R-param-ids param-ids)
          (error "prototype is not matched with k&r-style params"))))
    (make-function-definition
     :lisp-code
     (let ((varargs-sym (gensym "varargs-"))
           (body (expand-toplevel-stat body))) 
       `((defun ,func-name (,@param-ids ,@(if variadic `(&rest ,varargs-sym)))
           ,(if variadic
                `(macrolet ((va_start (ap &optional last)
                              (declare (ignore last))
                              `(setf ,ap ,',varargs-sym))
                            (va_arg (ap &optional type)
                              (declare (ignore type))
                              `(pop ,ap))
                            (va_end (ap)
                              `(setf ,ap nil))
                            (va_copy (dest src)
                              `(setf ,dest (copy-list ,src))))
                   ,body)
                body))))
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
                                decls codes))))

;;; The parser
(define-parser *expression-parser*
  (:muffle-conflicts t)         ; for 'dangling else'.
  ;; http://www.swansontec.com/sopc.html
  (:precedence (;; Primary expression
		(:left \( \) [ ] \. -> ++ --)
		;; Unary
		(:right * & + - ! ~ ++ -- #+ignore(typecast) |sizeof|)
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
		(:left \,)))
  ;; http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf
  (:terminals
   #.(append +operators+
	     +keywords+
	     '(enumeration-const id typedef-id
	       int-const char-const float-const
	       string)
	     '(lisp-expression)))
  (:start-symbol wcs-entry-point)

  ;; Our entry point.
  ;; top level forms in C, or statements
  (wcs-entry-point
   (translation-unit
    #'(lambda (us) (expand-translation-unit us)))
   (labeled-stat
    #'(lambda (st) (expand-toplevel-stat st)))
   ;; exp-stat is not included, because it is gramatically ambiguous.
   (compound-stat
    #'(lambda (st) (expand-toplevel-stat st)))
   (selection-stat
    #'(lambda (st) (expand-toplevel-stat st)))
   (iteration-stat
    #'(lambda (st) (expand-toplevel-stat st)))
   (jump-stat
    #'(lambda (st) (expand-toplevel-stat st))))


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

  ;; returns a 'decl-specs' structure
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
   |auto| |register| |static| |extern| |typedef|) ; keywords

  (type-spec
   |void| |char| |short| |int| |long|   ; keywords
   |float| |double| |signed| |unsigned|
   struct-or-union-spec
   enum-spec
   typedef-name)

  (type-qualifier
   |const| |volatile|)                  ; keywords

  ;; returns a struct-or-union-spec structure
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
   |struct| |union|)                        ; keywords

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

  ;; returns an init-declarator structure
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

  ;; returns a spec-qualifier-list structure
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

  ;; returns a struct-declarator structure
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

  ;; returns an enum-spec structure
  (enum-spec
   (|enum| id { enumerator-list }
         #'(lambda (_kwd id _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :id id :enumerator-list lis)))
   (|enum|    { enumerator-list }
         #'(lambda (_kwd _l lis _r)
             (declare (ignore _kwd _l _r))
	     (make-enum-spec :enumerator-list lis)))
   (|enum| id
         #'(lambda (_kwd id)
             (declare (ignore _kwd))
	     (make-enum-spec :id id))))

  (enumerator-list
   (enumerator
    #'list)
   (enumerator-list \, enumerator
                    #'concatinate-comma-list))

  ;; returns an enumerator structure
  (enumerator
   (id
    #'(lambda (id)
	(make-enumerator :declarator id)))
   (id = const-exp
       #'(lambda (id _op exp)
           (declare (ignore _op))
	   (make-enumerator :declarator id :initializer exp))))

  ;; returns like:
  ;; (name (:aref nil) (:funcall nil) (:aref 5) (:funcall int))
  ;; processed in 'expand-init-declarator-init'
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

  ;; see 'direct-declarator'
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

  (typedef-name
   typedef-id)


  ;;; Statements: 'stat' structure
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
   (|case| const-exp \: stat
       #'(lambda (_k  exp _c stat)
	   (declare (ignore _k _c))
	   (push-case-label exp stat)
	   stat))
   (|default| \: stat
       #'(lambda (_k _c stat)
	   (declare (ignore _k _c))
	   (push-case-label '|default| stat)
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
   (|if| \( exp \) stat
       #'(lambda (op lp exp rp stat)
	   (declare (ignore op lp rp))
	   (extract-if-statement exp stat)))
   (|if| \( exp \) stat |else| stat
       #'(lambda (op lp exp rp stat1 el stat2)
	   (declare (ignore op lp rp el))
	   (extract-if-statement exp stat1 stat2)))
   (|switch| \( exp \) stat
	   #'(lambda (_k _lp exp _rp stat)
	       (declare (ignore _k _lp _rp))
	       (extract-switch exp stat))))

  (iteration-stat
   (|while| \( exp \) stat
	  #'(lambda (_k _lp cond _rp body)
	      (declare (ignore _k _lp _rp))
	      (extract-loop body :cond cond)))
   (|do| stat |while| \( exp \) \;
     #'(lambda (_k1 body _k2 _lp cond _rp _t)
	 (declare (ignore _k1 _k2 _lp _rp _t))
	 (extract-loop body :cond cond :post-test-p t)))
   (|for| \( exp \; exp \; exp \) stat
	#'(lambda (_k _lp init _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :cond cond :step step)))
   (|for| \( exp \; exp \;     \) stat
	#'(lambda (_k _lp init _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :cond cond)))
   (|for| \( exp \;     \; exp \) stat
	#'(lambda (_k _lp init _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init :step step)))
   (|for| \( exp \;     \;     \) stat
	#'(lambda (_k _lp init _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :init init)))
   (|for| \(     \; exp \; exp \) stat
	#'(lambda (_k _lp      _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :cond cond :step step)))
   (|for| \(     \; exp \;     \) stat
	#'(lambda (_k _lp      _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :cond cond)))
   (|for| \(     \;     \; exp \) stat
	#'(lambda (_k _lp      _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body :step step)))
   (|for| \(     \;     \;     \) stat
	#'(lambda (_k _lp      _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (extract-loop body))))

  (jump-stat
   (|goto| id \;
	 #'(lambda (_k id _t)
	     (declare (ignore _k _t))
	     (make-stat :code (list `(go ,id)))))
   (|continue| \;
	     #'(lambda (_k _t)
		 (declare (ignore _k _t))
		 (make-stat-unresolved-continue)))
   (|break| \;
	  #'(lambda (_k _t)
	      (declare (ignore _k _t))
	      (make-stat-unresolved-break)))
   (|return| exp \;
	   #'(lambda (_k exp _t)
	       (declare (ignore _k _t))
	       ;; use the block of PROG
	       (make-stat :code (list `(return ,exp)))))
   (|return| \;
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
	      (lispify-binary 'mulf))
   (unary-exp /= assignment-exp
	      (lispify-binary 'divf))
   (unary-exp %= assignment-exp
	      (lispify-binary 'modf))
   (unary-exp += assignment-exp
	      (lispify-binary 'incf))
   (unary-exp -= assignment-exp
	      (lispify-binary 'decf))
   (unary-exp <<= assignment-exp
	      (lispify-binary 'ashf))
   (unary-exp >>= assignment-exp
	      (lispify-binary 'reverse-ashf))
   (unary-exp &= assignment-exp
	      (lispify-binary 'logandf))
   (unary-exp ^= assignment-exp
	      (lispify-binary 'logxorf))
   (unary-exp \|= assignment-exp
	      (lispify-binary 'logiorf)))

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
		     (lispify-binary 'reverse-ash)))

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
   (\( type-name \) cast-exp
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
	  (lispify-address-of exp)))
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
   (~ cast-exp
      (lispify-unary 'lognot))
   (|sizeof| unary-exp
	   #'(lambda (_op exp)
	       (declare (ignore _op))
	       ;; calculate runtime
	       `(if (arrayp ,exp)
		    (array-total-size ,exp)
		    1)))
   (|sizeof| \( type-name \)
	   #'(lambda (_op _lp tp _rp)
	       (declare (ignore _op _lp _rp))
	       ;; calculate compile-time
	       (if (subtypep tp 'array)
		   (destructuring-bind (a-type &optional e-type array-dim) tp
		     (declare (ignore a-type e-type))
		     (when (member-if-not #'numberp array-dim)
		       (error "The array dimension is incompleted: ~S" tp))
		     (apply #'* array-dim))
		   1))))

  (postfix-exp
   primary-exp
   (postfix-exp [ exp ]
		#'(lambda (exp op1 idx op2)
		    (declare (ignore op1 op2))
                    (if (and (listp exp) (eq (first exp) 'wcs-aref))
			(append-item-to-right exp idx)
                        `(wcs-aref ,exp ,idx))))
   (postfix-exp \( argument-exp-list \)
		#'(lambda (exp op1 args op2)
		    (declare (ignore op1 op2))
                    (lispify-funcall exp args)))
   (postfix-exp \( \)
		#'(lambda (exp op1 op2)
		    (declare (ignore op1 op2))
                    (lispify-funcall exp nil)))
   (postfix-exp \. id
		#'(lambda (exp _op id)
		    (declare (ignore _op))
		    `(wcs-struct-field ,exp ',id)))
   (postfix-exp -> id
		#'(lambda (exp _op id)
		    (declare (ignore _op))
		    `(wcs-struct-field (pseudo-pointer-dereference ,exp) ',id)))
   (postfix-exp ++
		#'(lambda (exp _op)
		    (declare (ignore _op))
		    `(post-incf ,exp 1)))
   (postfix-exp --
		#'(lambda (exp _op)
		    (declare (ignore _op))
		    `(post-incf ,exp -1))))

  (primary-exp
   id
   const
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

  (const
   int-const
   char-const
   float-const
   enumeration-const)
  )

;;; Expander
(defun c-expression-tranform (form symbol-case)
  (with-new-wcs-environment ()
    (parse-with-lexer (list-lexer form symbol-case)
                      *expression-parser*)))

;;; Macro interface
(defmacro with-c-syntax (() &body body)
  (cond ((null body)
	 nil)
	((and (length= 1 body) (listp (first body)))
	 ;; for using a reader.
	 (c-expression-tranform (first body)
                                (or (getf (first *current-c-reader*) :case)
                                    (readtable-case *readtable*))))
	(t
	 (c-expression-tranform body (readtable-case *readtable*)))))
