(in-package #:with-c-syntax.core)

;;; Variables
(defvar *dynamic-binding-requested* nil
  "* Value Type 
a list :: consists of symbols.

* Description
Holds a list of symbols, which are pointed by a pointer.
If a pseudo-pointer is created for a symbol, the symbol is added to
here (Because such a symbol must be handled carefully).

* Notes
At the beginning of ~with-c-syntax~, it binds this variable to nil.

* Affected By
~with-c-compilation-unit~.
")

(defvar *function-pointer-ids* nil
  "* Value Type
a list :: consists of symbols.

* Description
Holds a list of symbols, which are declared as a pointer
to a function.  (Because such a symbol is specially treated by the
function-calling expression.)

* Notes
At the beginning of ~with-c-syntax~, it binds this variable to nil.

* Affected By
~with-c-compilation-unit~.
")

(defvar *toplevel-entry-form* nil
  "* Value Type
a list

* Description
Holds a form inserted as an entry point.

This is used only when compiling a translation unit. Not used for
other cases.

* Notes
At the beginning of ~with-c-syntax~, it binds this variable depending
on its ~return~ argument.

* Affected By
~with-c-compilation-unit~.
")

(defvar *return-last-statement* t
  "* Value Type
a boolean

* Description
Specifies which to return the last form's value of compound statements.

* Notes
At the beginning of ~with-c-syntax~, it binds this variable depending
on its ~return~ argument.

* Affected By
~with-c-compilation-unit~.
")

(defvar *wcs-expanding-environment* nil
  "`with-c-syntax' bind this to `&environment' argument.")

;;; Lexer
(defun list-lexer (list &aux (syntax-package (find-syntax-package)))
  #'(lambda ()
      (let ((value (pop list)))
        (typecase value
          (null
           (if list
               (values 'id nil)
               (values nil nil)))
          (symbol
           (cond ((eql (symbol-package value) syntax-package)
                  ;; They must be belongs this package.
                  ;; (done by the preprocessor)
                  (values value value))
                 ((gethash value *typedef-names*)
                  (values 'typedef-id value))
                 (t
                  (values 'id value))))
          (integer
           (values 'int-const value))
          (character
           (values 'char-const value))
          (float
           (values 'float-const value))
          (string
           (values 'string value))
          (otherwise
           (values 'lisp-expression value))))))

;;; Declarations
(defstruct decl-specs
  "Represents 'decl-specs' in C syntax BNF."
  ;; Filled by the parser
  (type-spec nil)
  (storage-class nil)
  (qualifier nil)
  ;; Filled by 'finalize-decl-specs', and referred by 'finalize-init-declarator'
  (lisp-type t)             ; typename for Common Lisp
  (tag nil)		    ; struct/union/enum tag
  (typedef-init-decl nil)   ; typedef
  ;; Filled by 'finalize-decl-specs', and referred by 'expand-toplevel'
  (enum-bindings nil)       ; enum definition
  (struct-spec nil))	    ; struct/union definition

(defmethod make-load-form ((obj decl-specs) &optional environment)
  (make-load-form-saving-slots obj
   :slot-names '(type-spec storage-class qualifier
   		 lisp-type tag typedef-init-decl)
   :environment environment))

(defstruct init-declarator
  "Represents 'init-declarator' in C syntax BNF."
  ;; Filled by the parser
  declarator
  (initializer nil)
  ;; Filled by 'finalize-init-declarator'
  (lisp-name)
  (lisp-initform)
  (lisp-type))

(defmethod make-load-form ((obj init-declarator) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defstruct struct-or-union-spec
  "Represents 'struct-or-union-spec' in C syntax BNF."
  type					; symbol. 'struct' or 'union'
  (id nil)
  (struct-decl-list nil)) ; alist of (spec-qualifier-list . (struct-declarator ...))

(defmethod make-load-form ((obj struct-or-union-spec) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defstruct (spec-qualifier-list
             (:include decl-specs))
  "Represents 'spec-qualifier-list' in C syntax BNF.")

(defmethod make-load-form ((obj spec-qualifier-list) &optional environment)
  (make-load-form-saving-slots obj
   :slot-names '(type-spec storage-class qualifier
   		 lisp-type tag typedef-init-decl)
   :environment environment))

(defstruct (struct-declarator
             (:include init-declarator))
  "Represents 'struct-declarator' in C syntax BNF."
  (bits nil))

(defmethod make-load-form ((obj struct-declarator) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defstruct enum-spec
  "Represents 'enum-spec' in C syntax BNF."
  (id nil)				; enum tag
  (enumerator-list nil))                ; list of enumerator

(defmethod make-load-form ((obj enum-spec) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defstruct (enumerator
	     (:include init-declarator))
  "Represents 'enumerator' in C syntax BNF.")

(defmethod make-load-form ((obj enumerator) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defun finalize-struct-or-union-spec (suspec dspecs)
  "Fills the decl-specs object referring the passed struct-or-union-spec.
If required, makes a new struct-spec object."
  (setf (decl-specs-tag dspecs) (or (struct-or-union-spec-id suspec)
				    (gensym "unnamed-struct-"))
	(decl-specs-lisp-type dspecs) 'struct)
  (loop for (spec-qual . struct-decls)
     in (struct-or-union-spec-struct-decl-list suspec)
     do (finalize-decl-specs spec-qual)
     ;; included definitions
     do (appendf (decl-specs-enum-bindings dspecs) 
		 (decl-specs-enum-bindings spec-qual))
     do (appendf (decl-specs-struct-spec dspecs) 
		 (decl-specs-struct-spec spec-qual))
     ;; this struct
     nconc
       (loop with tp = (decl-specs-lisp-type spec-qual)
	  with constness = (member '|const| (decl-specs-qualifier spec-qual))
	  for s-decl in struct-decls
	  as (decl-name . abst-decl) = (init-declarator-declarator s-decl)
	  as name = (or decl-name (gensym "unnamed-member-"))
	  as initform = (expand-init-declarator-init spec-qual abst-decl nil)
	  as bits = (struct-declarator-bits s-decl)
	  ;; NOTE: In C, max bits are limited to the normal type.
	  ;; http://stackoverflow.com/questions/2647320/struct-bitfield-max-size-c99-c
	  if (and bits
		  (not (subtypep `(signed-byte ,bits) tp))
		  (not (subtypep `(unsigned-byte ,bits) tp)))
	  do (error 'compile-error
                    :format-control "Invalid bitfield specified: ~A, ~A."
                    :format-arguments (list tp s-decl))
	  collect (list :lisp-type tp :constness constness
			:name name :initform initform
			:decl-specs spec-qual
                        :abst-declarator abst-decl))
     into member-defs
     finally
       (when member-defs
	 ;; Now defines a new struct
	 (let ((sspec (make-struct-spec
		       :struct-name (decl-specs-tag dspecs)
                       :union-p (eq (struct-or-union-spec-type suspec) '|union|)
		       :member-defs member-defs
		       :defined-in-this-unit t)))
	   (add-struct-spec (decl-specs-tag dspecs) sspec)
	   ;; This struct-spec is treated by this dspecs
	   (push-right (decl-specs-struct-spec dspecs) sspec))))
  dspecs)

(deftype enum ()
  "Represents the enum type."
  'fixnum)

(defun finalize-enum-spec (espec dspecs)
  "Fills the decl-specs object referring the passed enum-spec."
  (setf (decl-specs-lisp-type dspecs) 'enum)
  (setf (decl-specs-tag dspecs)
	(or (enum-spec-id espec) (gensym "unnamed-enum-")))
  ;; adds values into lisp-decls
  (setf (decl-specs-enum-bindings dspecs)
	(loop as default-initform = 0 then `(1+ ,e-decl)
	   for e in (enum-spec-enumerator-list espec)
	   as e-decl = (init-declarator-declarator e)
	   as e-init = (init-declarator-initializer e)
	   collect (list e-decl (or e-init default-initform))))
  dspecs)

(define-constant +numeric-types-alist+
    '(;; Extension: uses T if no types are specified
      (()				.	t)
      ;; Integers
      ((|int|)                          .	fixnum)
      ((|int| |short|)                  .	(signed-byte 16))
      ((|int| |long|)                   .	(signed-byte 32))
      ((|int| |long| |long|)            .	(signed-byte 64))
      ((|int| |signed|)                 .	fixnum)
      ((|int| |short| |signed|)         .	(signed-byte 16))
      ((|int| |long| |signed|)          .	(signed-byte 32))
      ((|int| |long| |long| |signed|)   .	(signed-byte 64))
      ((|int| |unsigned| )              .	(integer 0 #.(max most-positive-fixnum 65535)))
      ((|int| |short| |unsigned|)       .	(unsigned-byte 16))
      ((|int| |long| |unsigned|)        .	(unsigned-byte 32))
      ((|int| |long| |long| |unsigned|) .	(unsigned-byte 64))
      ;; Integers, but 'int' is emitted
      ((|short|)			.	(signed-byte 16))
      ((|long|)				.	(signed-byte 32))
      ((|long| |long|)			.	(signed-byte 64))
      ((|signed|)			.	fixnum)
      ((|short| |signed|)		.	(signed-byte 16))
      ((|long| |signed|)		.	(signed-byte 32))
      ((|long| |long| |signed|)		.	(signed-byte 64))
      ((|unsigned| )			.	(integer 0 #.(max most-positive-fixnum 65535)))
      ((|short| |unsigned|)		.	(unsigned-byte 16))
      ((|long| |unsigned|)		.	(unsigned-byte 32))
      ((|long| |long| |unsigned|)	.	(unsigned-byte 64))
      ;; Char
      ((|char|)                         .	character)
      ((|char| |signed|)                .	(signed-byte 8))
      ((|char| |unsigned|)              .	(unsigned-byte 8))
      ;; Float
      ((|float|)                        .	single-float)
      ((|float| |short|)                .	short-float)
      ((|double|)                       .	double-float)
      ((|double| |long|)                .	long-float))
  :test 'equal
  :documentation
  "* Value Type
a list :: consists of alists -- (list-of-symbols . <lisp-type>)

* Description
Holds relationships between notations of C type and Common Lisp types.

* Notes
For each entry of alist, the car is sorted alphabetically.
")

(defun finalize-type-spec (dspecs)
  "A part of finalize-decl-specs. This processes type-spec."
  (loop with numeric-symbols = nil
     with tp-list of-type list = (decl-specs-type-spec dspecs)
     initially
       (when (null tp-list)
	 (return dspecs))
     for tp in tp-list
     do (flet ((check-tp-list-length ()
		 (unless (length= 1 tp-list)
		   (error 'compile-error
                          :format-control "Invalid decl-spec: ~A."
                          :format-arguments (list tp-list)))))
	  (cond
	    ((eq tp '|void|)		; void
	     (check-tp-list-length)
	     (setf (decl-specs-lisp-type dspecs) nil)
	     (return dspecs))
	    ((struct-or-union-spec-p tp)	; struct / union
	     (check-tp-list-length)
	     (return (finalize-struct-or-union-spec tp dspecs)))
	    ((enum-spec-p tp)		; enum
	     (check-tp-list-length)
	     (return (finalize-enum-spec tp dspecs)))
	    ((listp tp)			; lisp type
	     (check-tp-list-length)
	     (assert (starts-with '|__lisp_type| tp))
	     (setf (decl-specs-lisp-type dspecs) (second tp))
	     (return dspecs))
	    ((find-typedef tp)		; typedef name
	     (check-tp-list-length)
	     (let ((td-dspecs (find-typedef tp)))
               (setf (decl-specs-lisp-type dspecs)
                     (decl-specs-lisp-type td-dspecs)
                     (decl-specs-tag dspecs)
                     (decl-specs-tag td-dspecs)
                     (decl-specs-typedef-init-decl dspecs)
                     (decl-specs-typedef-init-decl td-dspecs))
               (return dspecs)))
	    (t				; numeric types
	     (push tp numeric-symbols))))
     finally
       (setf numeric-symbols (sort numeric-symbols #'string<))
       (setf (decl-specs-lisp-type dspecs)
             (if-let ((n-entry (assoc numeric-symbols
                                      +numeric-types-alist+
                                      :test #'equal)))
               (cdr n-entry)
               (error 'compile-error
                      :format-control "Invalid numeric type: ~A."
                      :format-arguments (list numeric-symbols))))
       (return dspecs)))

(defun finalize-decl-specs (dspecs)
  "Checks and fills the passed decl-specs."
  (finalize-type-spec dspecs)
  (setf (decl-specs-qualifier dspecs)
	(remove-duplicates (decl-specs-qualifier dspecs)))
  (setf (decl-specs-storage-class dspecs)
	(if (> (length (decl-specs-storage-class dspecs)) 1)
	    (error 'compile-error
                   :format-control "Too many storage-class specified: ~A."
                   :format-arguments (list (decl-specs-storage-class dspecs)))
	    (first (decl-specs-storage-class dspecs))))
  dspecs)

(defun array-dimension-combine (array-dimension-list init)
  "Resolves unspecified dimensions with an initializer."
  (loop with init-dims = (etypecase init
                           (list (dimension-list-max-dimensions init))
                           (array (array-dimensions init))) 
     for a-elem in array-dimension-list
     for i-elem = (pop init-dims)
     if (null i-elem)
     collect a-elem
     else if (eq a-elem '*)
     collect i-elem
     else if (<= i-elem a-elem)
     collect a-elem
     else
     do (warn 'with-c-syntax-warning
              :format-control "Too much elements in an initializer: (~S, ~S)."
              :format-arguments (list array-dimension-list init))
     and collect a-elem))

(defun setup-init-list (dims dspecs abst-declarator init)
  "Makes a list for `:initial-contents' of `make-array', from initializer-list."
  (let* ((default (expand-init-declarator-init dspecs
                                               (nthcdr (length dims) abst-declarator)
                                               nil))
         (ret (make-dimension-list dims default)))
    (labels ((var-init-setup (rest-dims subscripts abst-decls init)
               (if (null rest-dims)
                   (setf (apply #'ref-dimension-list ret subscripts)
                         (expand-init-declarator-init dspecs abst-decls init))
                   (loop for d from 0 below (car rest-dims)
                      for init-i in init
                      initially (assert (starts-with :aref (car abst-decls)))
                      do (var-init-setup (cdr rest-dims)
                                         (add-to-tail subscripts d)
                                         (cdr abst-decls) init-i)))))
      (var-init-setup dims () abst-declarator init))
    ret))

(defun expand-init-declarator-init (dspecs abst-declarator initializer
                                    &key (allow-incomplete nil))
  "Finds the specified type and the initialization form.
Returns (values var-init var-type)."
  (flet ((error-on-incomplete (datum &rest args)
           (unless allow-incomplete
             (apply #'error datum args))))
    (ecase (car (first abst-declarator))
      (:pointer
       (if (and (null (cdr abst-declarator)) ; 'void*' check
                (equal (decl-specs-type-spec dspecs) '(|void|)))
           (values initializer t)
           (let ((next-type
                  (nth-value 1 (expand-init-declarator-init
                                dspecs (cdr abst-declarator) nil
                                :allow-incomplete t))))
             (values (or initializer nil)
                     `(pseudo-pointer ,next-type)))))
      (:funcall
       (case (car (second abst-declarator))
         (:aref 
          (error 'compile-error
                 :format-control "A function returning an array is not accepted."))
         (:funcall
          (error 'compile-error
                 :format-control "A function returning a function is not accepted.")))
       (when initializer
         (error 'compile-error
                :format-control "A function cannot take an initializer."))
       ;; TODO: includes returning type, and arg type
       (values nil 'function))
      (:aref
       (let* ((aref-type (decl-specs-lisp-type dspecs))
              (aref-dim                 ; reads abst-declarator
               (loop for (tp tp-args) in abst-declarator
                  collect
                    (case tp
                      (:funcall
                       (error 'compile-error
                              :format-control "An array of functions is not accepted."))
                      (:aref (or tp-args '*))
                      (:pointer
                       (setf aref-type `(pseudo-pointer ,aref-type))
                       (loop-finish))
                      (otherwise
                       (assert nil () "Unexpected internal type: ~S." tp)))))
              (merged-dim
               (array-dimension-combine aref-dim initializer))
              (lisp-elem-type
               (cond ((subtypep aref-type 'number) aref-type)
                     ((subtypep aref-type 'character) aref-type)
                     (t t)))            ; excludes compound types 
              (var-type
               (if (and (or (null aref-dim) (member '* aref-dim))
                        (null initializer))
                   (error-on-incomplete
                    'compile-error
                    :format-control "Array's dimension is not fully specified: (~S, ~S)."
                    :format-arguments (list aref-dim initializer))
                   `(simple-array ,lisp-elem-type ,merged-dim)))
              (var-init
               (if (and (arrayp initializer)
                        (equal (array-dimensions initializer) merged-dim))
                   ;; This path is for 'char foo[] = "bar..";' syntax.
                   ;; FIXME: To support an array of string, change `setup-init-list' function.
                   initializer 
                   `(make-array ',merged-dim
                                :element-type ',lisp-elem-type
                                :initial-contents
                                ,(make-dimension-list-load-form
                                  (setup-init-list merged-dim dspecs
                                                   abst-declarator initializer)
                                  (length merged-dim))))))
         (values var-init var-type)))
      ((nil)
       (let ((var-type (decl-specs-lisp-type dspecs)))
         (cond
           ((type= var-type nil)
            (error 'compile-error
                   :format-control "A void variable cannot be initialized."))
           ((type= var-type 't)
            (values initializer var-type))
           ((subtypep var-type 'number) ; includes enum
            (values (or initializer 0) var-type))
           ((subtypep var-type 'struct)
            (let* ((sspec (find-struct-spec (decl-specs-tag dspecs)))
                   (var-init
                    (if (not sspec)
                        (error-on-incomplete
                         'compile-error
                         :format-control "A struct named ~S is not defined."
                         :format-arguments (list (decl-specs-tag dspecs)))
                        `(make-struct
                          ,(if (struct-spec-defined-in-this-unit sspec)
                               (find-struct-spec (struct-spec-struct-name sspec))
                               `',(struct-spec-struct-name sspec))
                          ,@(loop for init in initializer
                               for mem in (struct-spec-member-defs sspec)
                               collect (expand-init-declarator-init
                                        (getf mem :decl-specs)
                                        (getf mem :abst-declarator)
                                        init))))))
              (values var-init var-type)))
           (t           ; unknown type. Maybe user supplied lisp-type.
            (values initializer var-type))))))))

(defun finalize-init-declarator (dspecs init-decl)
  "Fills the passed init-declarator object."
  (let* ((decl (init-declarator-declarator init-decl))
         (init (init-declarator-initializer init-decl))
         (var-name (first decl))
         (abst-decl (rest decl))
	 (storage-class (decl-specs-storage-class dspecs)))
    (when (and init
               (member storage-class '(|extern| |typedef|)))
      (error 'compile-error
             :format-control "This storage-class (~S) variable cannot have any initializers."
             :format-arguments (list storage-class)))
    ;; If not typedef-ing, expands typedef contents.
    (unless (eq storage-class '|typedef|)
      (when-let (td-init-decl (decl-specs-typedef-init-decl dspecs))
        (appendf abst-decl
                 (cdr (init-declarator-declarator td-init-decl)))))
    (multiple-value-bind (var-init var-type)
	(expand-init-declarator-init dspecs abst-decl init)
      (when (and (subtypep var-type 'function)
                 (not (member storage-class '(nil |extern| |static|))))
        (error 'compile-error
               :format-control "A function cannot have this storage-class: ~S."
               :format-arguments (list storage-class)))
      (setf (init-declarator-lisp-name init-decl) var-name
	    (init-declarator-lisp-initform init-decl) var-init
	    (init-declarator-lisp-type init-decl) var-type)
      (when (and (subtypep var-type 'pseudo-pointer)
                 (starts-with 'pseudo-pointer var-type)
                 (subtypep (second var-type) 'function))
        (push var-name *function-pointer-ids*)))
    (when (eq '|typedef| storage-class)
      (setf (decl-specs-typedef-init-decl dspecs) init-decl)
      (add-typedef var-name dspecs))
    init-decl))

;;; Expressions
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; These are directly called by the parser..
(defun concatenate-comma-list (lis op i)
  (declare (ignore op))
  (add-to-tail lis i))

(defun lispify-unary (op)
  #'(lambda (_ exp)
      (declare (ignore _))
      `(,op ,exp)))

(defun lispify-binary (op)
  #'(lambda (exp1 _ exp2)
      (declare (ignore _))
      `(,op ,exp1 ,exp2)))
)

(defun lispify-type-name (spec-qual abs)
  (finalize-decl-specs spec-qual)
  (if abs
      (let ((init-decl (make-init-declarator :declarator abs)))
	(finalize-init-declarator spec-qual init-decl)
        (init-declarator-lisp-type init-decl))
      (decl-specs-lisp-type spec-qual)))

(defun error-lisp-subscript (obj)
  (error 'runtime-error
         :format-control "This object cannot have any subscripts: ~S."
         :format-arguments (list obj)))

(defun lisp-subscript (obj arg1 &rest args)
  (typecase obj
    (pseudo-pointer
     (let ((deref-obj (pseudo-pointer-dereference (+ obj arg1))))
       (if (null args)
           deref-obj
           (apply #'lisp-subscript deref-obj args))))
    (array
     (apply #'aref obj arg1 args))
    (otherwise
     (error-lisp-subscript obj))))

(defun (setf lisp-subscript) (val obj arg1 &rest args)
  (typecase obj
    (pseudo-pointer
     (symbol-macrolet 
         ((deref-obj (pseudo-pointer-dereference (+ obj arg1))))
       (if (null args)
           (setf deref-obj val)
           (setf (apply #'lisp-subscript deref-obj args) val))))
    (array
     (setf (apply #'aref obj arg1 args) val))
    (otherwise
     (error-lisp-subscript obj))))

(defun lispify-cast (type exp)
  (if (null type)
      `(progn ,exp (values))            ; like '(void)x;'
      `(coerce ,exp ',type)))

(defun lispify-address-of (exp)
  (flet ((error-bad-form ()
           (error 'compile-error
                  :format-control "Cannot take a pointer to form ~S."
                  :format-arguments (list exp))))
    (cond ((symbolp exp)
           (push exp *dynamic-binding-requested*)
           (once-only ((val exp))
             `(make-pseudo-pointer
               (if (pseudo-pointer-pointable-p ,val)
                   ,val ',exp))))
          ((listp exp)
           (destructuring-case exp
             ((lisp-subscript obj &rest args)
              (once-only (obj)
                `(if (arrayp ,obj)
                     (make-pseudo-pointer
                      (make-reduced-dimension-array ,obj ,@(butlast args))
                      ,(lastcar args))
                     (error 'runtime-error
                            :format-control "Trying to get a pointer to an array, but this is not an array: ~S."
                            :format-arguments (list ,obj)))))
             ((struct-member obj mem)
              (once-only (obj)
                `(if (typep ,obj 'struct)
                     (make-pseudo-pointer
                      (struct-member-vector ,obj)
                      (struct-member-index ,obj ,mem))
                     (error 'runtime-error
                            :format-control "Trying to get a pointer to a struct member, but this is not a struct: ~S."
                            :format-arguments (list ,obj)))))
             ((pseudo-pointer-dereference obj)
              obj)
             ((otherwise &rest _)
              (declare (ignore _))
              (error-bad-form))))
          (t
           (error-bad-form)))))

(defun lispify-funcall (func-exp args)
  (if (and (symbolp func-exp)
           (not (member func-exp *function-pointer-ids*)))
      `(,func-exp ,@args)
      `(funcall ,func-exp ,@args)))

(defun lispify-offsetof (dspecs id)
  (finalize-decl-specs dspecs)
  (when-let* ((tag (decl-specs-tag dspecs))
              (sspec (find-struct-spec tag))
              (entry
	       (loop for mem in (struct-spec-member-defs sspec)
                  until (eq (getf mem :name) id)
                  count mem)))
    (return-from lispify-offsetof entry))
  (error 'compile-error
         :format-control "Bad 'offsetof' usage."))

;;; Statements
(defstruct stat
  "Represents statements in C syntax BNF."
  (code nil)
  (declarations nil)        ; list of 'init-declarator'
  (break-statements nil)    ; list of (go 'break), should be rewrited
  (continue-statements nil) ; list of (go 'continue), should be rewrited
  (case-label-list nil))    ; alist of (<gensym> . :exp <case-exp>)

(defun merge-stat (s1 s2 &key (empty-code nil))
  (make-stat :code (if empty-code nil
		       (append (stat-code s1)
			       (stat-code s2)))
	     :declarations (append (stat-declarations s1)
				   (stat-declarations s2))
	     :break-statements (append (stat-break-statements s1)
				       (stat-break-statements s2))
	     :continue-statements (append (stat-continue-statements s1)
					  (stat-continue-statements s2))
	     :case-label-list (append (stat-case-label-list s1)
				      (stat-case-label-list s2))))

(defun expand-if-statement (exp then-stat
			     &optional (else-stat nil))
  (let* ((stat (if else-stat
		   (merge-stat then-stat else-stat :empty-code t)
		   then-stat))
	 (else-tag (gensym "if-else-")) ; TODO: remove this if not else-stat?
	 (end-tag (gensym "if-end-")))
    (setf (stat-code stat)
	  `((unless ,exp (go ,else-tag))
	    ,@(stat-code then-stat)     ; then
	    (go ,end-tag)
	    ,else-tag                   ; else
	    ,@(if else-stat (stat-code else-stat) nil)
	    ,end-tag))                  ; end
    stat))

(defun make-stat-unresolved-break ()
  ;; Because of rewriting, the list of '(go ...)' must be fresh.
  (let ((ret (list 'go (gensym "unresolved-break-"))))
    (make-stat :code (list ret)
	       :break-statements (list ret))))

(defun rewrite-break-statements (sym stat)
  (let ((ret (shiftf (stat-break-statements stat) nil)))
    (dolist (i ret ret)
      (setf (second i) sym))))

(defun make-stat-unresolved-continue ()
  ;; Because of rewriting, the list of '(go ...)' must be fresh.
  (let ((ret (list 'go (gensym "unresolved-continue-"))))
    (make-stat :code (list ret)
	       :continue-statements (list ret))))

(defun rewrite-continue-statements (sym stat)
  (let ((ret (shiftf (stat-continue-statements stat) nil)))
    (dolist (i ret ret)
      (setf (second i) sym))))

(defun expand-loop (body-stat
		     &key (init nil) (cond t) (step nil)
		     (post-test-p nil))
  (let* ((loop-body-tag (gensym "loop-body-"))
	 (loop-step-tag (gensym "loop-step-"))
	 (loop-cond-tag (gensym "loop-cond-"))
	 (loop-end-tag (gensym "loop-end-"))
	 (used-breaks (rewrite-break-statements loop-end-tag body-stat))
	 (used-continues (rewrite-continue-statements loop-step-tag body-stat)))
    (setf (stat-code body-stat)
	  `((progn ,init)
	    ,(if post-test-p
		 `(go ,loop-body-tag)		; do-while
		 `(go ,loop-cond-tag))
	    ,loop-body-tag
	    ,@(stat-code body-stat)
	    ,@(if used-continues
		  `(,loop-step-tag))
	    (progn ,step)
	    ,@(if post-test-p
		  nil
		  `(,loop-cond-tag))
	    (when (progn ,cond)
	      (go ,loop-body-tag))
	    ,@(if used-breaks
		  `(,loop-end-tag))))
    body-stat))

(defun push-case-label (case-label-exp stat)
  (let ((go-tag-sym (gensym (format nil "case-~A-" case-label-exp))))
    (push (cons go-tag-sym case-label-exp)
          (stat-case-label-list stat))
    (push go-tag-sym (stat-code stat))))

(defun expand-switch (exp stat)
  (let* ((switch-end-tag (gensym "switch-end-"))
	 (default-supplied nil)
	 (jump-table			; create jump table with COND
	  (loop with default-clause = `(otherwise (go ,switch-end-tag))
	     for (go-tag-sym . case-label-exp)
	     in (shiftf (stat-case-label-list stat) nil)

	     if (eq case-label-exp '|default|)
	     do (setf default-clause `(otherwise (go ,go-tag-sym))
		      default-supplied t)
	     else
	     collect `(,case-label-exp (go ,go-tag-sym))
	     into clauses
	     finally
               (return `(case ,exp
                          ,@clauses
                          ,default-clause))))
	 (used-breaks (rewrite-break-statements switch-end-tag stat)))
    (setf (stat-code stat)
	  `(,jump-table
	    ,@(stat-code stat)
	    ,@(if (or used-breaks
		      (not default-supplied))
		  `(,switch-end-tag))))
    stat))

;;; Translation Unit -- function definitions
(defstruct function-definition
  "Represents a function definition."
  func-name
  storage-class
  func-args
  func-body
  lisp-type)

(defmacro get-variadic-arguments (&optional last-argument-name)
  "It returns the variadic arguments of a function defined by
with-c-syntax.  If this is called outside of a variadic function, an
error is signaled.  When defining a variadic function, a local macro
has same name is established.

This is not intended for calling directly. The `va_start' macro uses this."
  (declare (ignore last-argument-name))
  '(error 'runtime-error
    :format-control "Trying to get a variadic args list out of a variadic func."))

(defun lispify-function-definition (name body
                                    &key K&R-decls (return (make-decl-specs)))
  (let* ((func-name (first name))
         (func-param (getf (second name) :funcall))
         (variadic nil)
	 (omitted nil)
         (param-ids
          (loop for p in func-param
             if (eq p '|...|)
             do (setf variadic t) (loop-finish)
             else
             collect
	       (or (first (second p))	; first of declarator.
		   (let ((var (gensym "omitted-arg-")))
		     (push var omitted)
		     var))))
	 (return (finalize-decl-specs return))
	 (storage-class
	  (case (decl-specs-storage-class return)
	    (|static| '|static|)
	    ((nil) '|global|)
	    (otherwise
             (error 'compile-error
                    :format-control "Cannot define a function of storage-class: ~S."
                    :format-arguments (list (decl-specs-storage-class return)))))))
    (when K&R-decls
      (loop for (dspecs init-decls) in K&R-decls
         as storage-class = (decl-specs-storage-class dspecs)
         unless (member storage-class
                        '(nil |auto| |register|) :test #'eq)
         do (error 'compile-error
                   :format-control "Invalid storage-class ~S for function arguments."
                   :format-arguments (list storage-class))
         nconc (mapcar #'init-declarator-lisp-name init-decls) into K&R-param-ids
         finally
           (unless (equal param-ids K&R-param-ids)
             (error 'compile-error
                    :format-control "Function prototype (~A) is not matched with k&r-style params (~A)."
                    :format-arguments (list K&R-param-ids param-ids)))))
    (let ((varargs-sym (gensym "varargs-"))
          (body (expand-toplevel-stat body))) 
      (make-function-definition
       :func-name func-name
       :storage-class storage-class
       :func-args `(,@param-ids ,@(if variadic `(&rest ,varargs-sym)))
       :func-body
       `((declare (ignore ,@omitted))
         ,(if variadic
              `(macrolet ((get-variadic-arguments (&optional (last-argument-name nil l-supplied-p))
                            "locally established `get-variadic-arguments' macro."
			    (when l-supplied-p
			      (unless (eql last-argument-name ',(lastcar param-ids))
				(warn "~A's last argument before '...' is ~A, but ~A was passed."
				      ',func-name ',(lastcar param-ids) last-argument-name)))
			    ',varargs-sym))
                 ,body)
              body))
       :lisp-type `(function ',(mapcar (constantly t) param-ids) ; TODO: use arg types.
                             ',(decl-specs-lisp-type return))))))

;;; Toplevel
(defun expand-toplevel-init-decls (mode dspecs init-decls
                                   dynamic-established-syms)
  "A part of `expand-toplevel'."
  (loop with storage-class = (or (decl-specs-storage-class dspecs)
				 (ecase mode
				   (:statement '|auto|)
				   (:translation-unit '|global|)))
     with qualifiers = (decl-specs-qualifier dspecs)
     with lexical-binds = nil
     with dynamic-extent-vars = nil
     with special-vars = nil
     with global-defs = nil
     with typedef-names = nil
     with funcptr-syms = nil
     with sym-macro-defs = nil
     with toplevel-defs = nil

     initially
       (when (member '|volatile| qualifiers)
	 (warn 'with-c-syntax-style-warning
	       :message "'volatile' qualifier is currently ignored"))

     for i in init-decls
     as name = (init-declarator-lisp-name i)
     as init = (init-declarator-lisp-initform i)

     ;; function declarations
     if (subtypep (init-declarator-lisp-type i) 'function)
     do (unless (or (null init) (zerop init))
          (error 'compile-error
                 :format-control "A function cannot have initializer (~S = ~S)."
                 :format-arguments (list name init)))
     else do
     ;; variables
       (when (member name *dynamic-binding-requested*)
         (push name dynamic-established-syms))
       (when (member name *function-pointer-ids*)
         (push name funcptr-syms))
       (ecase storage-class
         (|auto|			; 'auto' vars
          (when (eq mode :translation-unit)
            (error 'compile-error
                   :format-control "At top level, 'auto' variables are not accepted (~S)."
                   :format-arguments (list name)))
          (push `(,name ,init) lexical-binds))
         (|register|			; 'register' vars
          (when (eq mode :translation-unit)
            (error 'compile-error
                   :format-control "At top level, 'register' variables are not accepted (~S)."
                   :format-arguments (list name)))
          (push `(,name ,init) lexical-binds)
          (when (member name dynamic-established-syms :test #'eq)
            (warn 'with-c-syntax-warning
                  :format-control "Variable ~A is 'register', but its pointer is taken."
                  :format-arguments (list name)))
          (push name dynamic-extent-vars))
         (|extern|			; 'extern' vars.
          (unless (or (null init) (zerop init))
            (error 'compile-error
                   :format-control  "An 'extern' variable cannot have initializer (~S = ~S)."
                   :format-arguments (list name init))))
         (|global|			; 'global' vars.
          (when (eq mode :statement)
            (error 'compile-error
                   :format-control "In internal scope, no global vars cannot be defined (~S)."
                   :format-arguments (list name)))
	  (if (constantp init *wcs-expanding-environment*)
	      (push `(defparameter ,name ,init
			 "generated by with-c-syntax, for global")
		      toplevel-defs)
	      ;; Temporary use a lexical value, for initializing correctly
	      ;; with static vars which are lexically bound.
	      (let ((init-sym (gensym (format nil "global-var-~A-tmp-" name))))
		(push `(,init-sym ,init) lexical-binds)
		(push `(defparameter ,name ,init-sym
			 "generated by with-c-syntax, for global and initialized by lexical variables.")
		      global-defs)))
	  (push name special-vars))
         (|static|			; 'static' vars.
	  (ecase mode
	    (:statement
	     ;; initialized only once.
	     (let ((st-sym (gensym (format nil "static-var-~A-storage-" name))))
	       (if (constantp init *wcs-expanding-environment*)
		   (push `(defvar ,st-sym ,init
			    "generated by with-c-syntax, for static.")
			 toplevel-defs)
		   (let ((st-init-sym (gensym (format nil "static-var-~A-tmp-" name))))
		     (push `(,st-init-sym ,init) lexical-binds)
		     (push `(defvar ,st-sym ,st-init-sym
			      "generated by with-c-syntax, for static and initialized by lexical variables.")
			   global-defs)))
	       (push st-sym special-vars)
	       (push `(,name ,st-sym) sym-macro-defs)))
	    (:translation-unit
	     ;; lexically bound
	     (push `(,name ,init) lexical-binds))))
         (|typedef|			; 'typedef' vars
          (push name typedef-names)
	  (when (eq mode :translation-unit)
	    (push `(add-typedef ',name ,(find-typedef name))
		  toplevel-defs))))
     finally
       (return
         (values (nreverse lexical-binds)
                 (nreverse dynamic-extent-vars)
                 (nreverse special-vars)
                 (nreverse global-defs)
		 (nreverse sym-macro-defs)
                 (nreverse typedef-names)
                 (nreverse funcptr-syms)
                 dynamic-established-syms
		 (nreverse toplevel-defs)))))

(defun expand-toplevel (mode decls fdefs code)
  "This is a final compilation phase. Makes a toplevel form.
~mode~ is one of :statement or :translation-unit"
  (let (;; used for :statement
        lexical-binds
        dynamic-extent-vars
	;; used for :translation-unit
        special-vars
        global-defs
	;; used for both
        func-defs
        local-funcs
	sym-macro-defs
        cleanup-typedef-names
        cleanup-funcptr-syms
        cleanup-struct-specs
        cleanup-dynamic-established-syms
	toplevel-defs)
    ;; process decls
    (loop for (dspecs init-decls) in decls
       ;; enum consts
       do (ecase mode
	    (:statement
             (revappendf lexical-binds (decl-specs-enum-bindings dspecs)))
	    (:translation-unit
	     (loop for (name val) in (decl-specs-enum-bindings dspecs)
		do (assert (constantp val *wcs-expanding-environment*))
                do (push `(defconstant ,name ,val
                            "generated by with-c-syntax, for global enum")
                         toplevel-defs))))
       ;; structs
       do (revappendf cleanup-struct-specs (decl-specs-struct-spec dspecs))
	 (loop for sspec in (decl-specs-struct-spec dspecs)
	    as sname = (struct-spec-struct-name sspec)
	    as defined-in ;; drops defined-in-this-unit flag here.
	      = (shiftf (struct-spec-defined-in-this-unit sspec) nil)
	    if (and defined-in
		    (eq mode :translation-unit))
            do (push `(add-struct-spec ',sname ,sspec)
                     toplevel-defs))
       ;; declarations
       do(multiple-value-bind 
               (lexical-binds-1 dynamic-extent-vars-1
                                special-vars-1 global-defs-1 sym-macro-defs-1
                                typedef-names-1 funcptr-syms-1
                                dynamic-established-syms-1
				toplevel-defs-1)
             (expand-toplevel-init-decls mode dspecs init-decls
                                         cleanup-dynamic-established-syms)
	   (assert (subsetp dynamic-extent-vars-1 lexical-binds-1
			    :test (lambda (dv lv)
				    (eql dv (car lv)))))
           (nreconcf lexical-binds lexical-binds-1)
           (nreconcf dynamic-extent-vars dynamic-extent-vars-1)
           (nreconcf special-vars special-vars-1)
           (nreconcf global-defs global-defs-1)
           (nreconcf sym-macro-defs sym-macro-defs-1)
           (nreconcf cleanup-typedef-names typedef-names-1)
           (nreconcf cleanup-funcptr-syms funcptr-syms-1)
           (setf cleanup-dynamic-established-syms dynamic-established-syms-1)
	   (nreconcf toplevel-defs toplevel-defs-1)))
    ;; functions
    (loop for fdef in fdefs
       as name = (function-definition-func-name fdef)
       as args = (function-definition-func-args fdef)
       as body = (function-definition-func-body fdef)
       do (ecase (function-definition-storage-class fdef)
            (|global|
             (push `(defun ,name ,args ,@body) func-defs))
            (|static|
             (push `(,name ,args ,@body) local-funcs))))
    ;; Finally, constructs a compiled form.
    (nreversef lexical-binds)
    (nreversef dynamic-extent-vars)
    (nreversef special-vars)
    (nreversef global-defs)
    (nreversef sym-macro-defs)
    (nreversef func-defs)
    (nreversef local-funcs)
    (nreversef cleanup-typedef-names)
    (nreversef cleanup-funcptr-syms)
    (nreversef cleanup-struct-specs)
    (nreversef toplevel-defs)
    (prog1
        `(progn
	   ,@toplevel-defs
	   (replace-operator-if-no-bindings locally
	     (let* (,@lexical-binds)
	       (declare (dynamic-extent ,@dynamic-extent-vars)
			(special ,@special-vars))
	       ;; 'global-defs' must be here, because some global variables may use
	       ;; 'static' variables, which are lexically bound.
	       ,@global-defs
	       (replace-operator-if-no-bindings progn
	         (labels (,@local-funcs)	; 'static' functions.
		   ,@func-defs		; 'global' functions.
		   (replace-operator-if-no-bindings progn
		     (with-dynamic-bound-symbols
			 ,(ecase mode
			    (:statement *dynamic-binding-requested*)
			    (:translation-unit nil))
		       (symbol-macrolet (,@sym-macro-defs)
			 ,@code))))))))
      ;; drop expanded definitions
      (loop for sym in cleanup-typedef-names
         do (remove-typedef sym))
      (loop for c in cleanup-struct-specs
         do (remove-struct-spec (struct-spec-struct-name c)))
      ;; drop symbols specially treated in this unit.
      (loop for sym in cleanup-dynamic-established-syms
         do (deletef *dynamic-binding-requested*
                     sym :test #'eq :count 1))
      (loop for sym in cleanup-funcptr-syms
         do (deletef *function-pointer-ids*
                     sym :test #'eq :count 1)))))

(defun expand-toplevel-stat (stat)
  (let* ((stat-codes (stat-code stat))
	 (last-form (lastcar stat-codes))
	 (ex-last-code
	  (if (and *return-last-statement*
		   (typecase last-form
		     (symbol
		      ;; uninterned symbols (gensym) are assumed as C labels.
		      (symbol-package last-form))
		     (list
		      ;; Don't wrap form if `return' already exists.
		      (not (eq (first last-form) 'return)))
		     (otherwise t)))
	      `(return ,last-form)
	      last-form))
	 (ex-code
	   `(block nil
	      (tagbody
		 ,@(butlast stat-codes)
		 ,ex-last-code))))
    (expand-toplevel :statement
		     (stat-declarations stat)
		     nil
		     `(,ex-code))))

(defun expand-translation-unit (units)
  (loop for u in units
     if (function-definition-p u)
     collect u into fdefs
     else
     collect u into decls
     finally
       (return (expand-toplevel :translation-unit
                                decls fdefs
				`(,*toplevel-entry-form*)))))

;;; The parser
(define-parser *expression-parser*
  (:muffle-conflicts t)         ; for 'dangling else'.
  ;; http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf
  (:terminals
   #.(append (loop for i being the external-symbol of (find-syntax-package)
		  collect i)
	     '(id typedef-id
	       int-const char-const float-const
	       string lisp-expression)))
  (:start-symbol wcs-entry-point)

  ;; Our entry point.
  ;; top level forms in C, or statements
  (wcs-entry-point
   (translation-unit
    ;; I require `lambda' for avoiding `eval-when' around `expand-translation-unit'
    #'(lambda (us) (expand-translation-unit us)))
   (labeled-stat
    #'(lambda (st) (expand-toplevel-stat st)))
   ;; exp-stat is not included, because it is grammatically ambiguous.
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
    #'add-to-tail))

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
               #'(lambda (dspecs inits _t)
                   (declare (ignore _t))
		   (finalize-decl-specs dspecs)
		   `(,dspecs
		     ,(mapcar #'(lambda (i) (finalize-init-declarator dspecs i))
                              inits))))
   (decl-specs \;
               #'(lambda (dspecs _t)
                   (declare (ignore _t))
		   (finalize-decl-specs dspecs)
		   `(,dspecs nil))))

  (decl-list
   (decl
    #'list)
   (decl-list decl
	      #'add-to-tail))

  ;; returns a 'decl-specs' structure
  (decl-specs
   (storage-class-spec decl-specs
                       #'(lambda (cls dspecs)
                           (push cls (decl-specs-storage-class dspecs))
                           dspecs))
   (storage-class-spec
    #'(lambda (cls)
	(make-decl-specs :storage-class `(,cls))))
   (type-spec decl-specs
              #'(lambda (tp dspecs)
                  (push tp (decl-specs-type-spec dspecs))
                  dspecs))
   (type-spec
    #'(lambda (tp)
	(make-decl-specs :type-spec `(,tp))))
   (type-qualifier decl-specs
                   #'(lambda (qlr dspecs)
                       (push qlr (decl-specs-qualifier dspecs))
                       dspecs))
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
   typedef-name
   (|__lisp_type| lisp-expression)      ; extension
   (|__lisp_type| id))                  ; extension

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
		     #'add-to-tail))

  (init-declarator-list
   (init-declarator
    #'list)
   (init-declarator-list \, init-declarator
                         #'concatenate-comma-list))

  ;; returns an init-declarator structure
  (init-declarator
   (declarator
    #'(lambda (d)
	(make-init-declarator :declarator d)))
   (declarator with-c-syntax.syntax:= initializer
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
			   #'concatenate-comma-list))

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
                    #'concatenate-comma-list))

  ;; returns an enumerator structure
  (enumerator
   (id
    #'(lambda (id)
	(make-enumerator :declarator id)))
   (id with-c-syntax.syntax:= const-exp
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
        (add-to-tail dcl `(:aref ,params))))
   (direct-declarator [		  ]
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcl '(:aref nil))))
   (direct-declarator \( param-type-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcl `(:funcall ,params))))
   (direct-declarator \( id-list \)
    #'(lambda (dcl _lp params _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcl `(:funcall
                           ;; make as a list of (decl-spec (id))
                           ,(mapcar #'(lambda (p) `(nil (,p))) params)))))
   (direct-declarator \(	 \)
    #'(lambda (dcl _lp _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcl '(:funcall nil)))))

  (pointer
   (with-c-syntax.syntax:* type-qualifier-list
    #'(lambda (_kwd qls)
        (declare (ignore _kwd))
        `((:pointer ,@qls))))
   (with-c-syntax.syntax:*
    #'(lambda (_kwd)
        (declare (ignore _kwd))
        '((:pointer))))
   (with-c-syntax.syntax:* type-qualifier-list pointer
    #'(lambda (_kwd qls ptr)
        (declare (ignore _kwd))
        (add-to-tail ptr `(:pointer ,@qls))))
   (with-c-syntax.syntax:*			pointer
    #'(lambda (_kwd ptr)
        (declare (ignore _kwd))
        (add-to-tail ptr '(:pointer)))))
			  

  (type-qualifier-list
   (type-qualifier
    #'list)
   (type-qualifier-list type-qualifier
			#'add-to-tail))

  (param-type-list
   param-list
   (param-list \, |...|
	       #'concatenate-comma-list))

  (param-list
   (param-decl
    #'list)
   (param-list \, param-decl
	       #'concatenate-comma-list))

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
    #'concatenate-comma-list))

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
    #'concatenate-comma-list))

  ;; see 'decl'
  (type-name
   (spec-qualifier-list abstract-declarator
			#'(lambda (spec-qual abs)
			    (lispify-type-name spec-qual abs)))
   (spec-qualifier-list
    #'(lambda (spec-qual)
	(lispify-type-name spec-qual nil))))

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
        (add-to-tail dcls `(:aref ,params))))
   (			       [ const-exp ]
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
        `((:aref ,params))))
   (direct-abstract-declarator [	   ]
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcls `(:aref nil))))
   (			       [	   ]
    #'(lambda (_lp _rp)
	(declare (ignore _lp _rp))
        '((:aref nil))))
   (direct-abstract-declarator \( param-type-list \)
    #'(lambda (dcls _lp params _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcls `(:funcall ,params))))
   (			       \( param-type-list \)
    #'(lambda (_lp params _rp)
	(declare (ignore _lp _rp))
        `((:funcall ,params))))
   (direct-abstract-declarator \(		  \)
    #'(lambda (dcls _lp _rp)
	(declare (ignore _lp _rp))
        (add-to-tail dcls '(:funcall nil))))
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
      #'(lambda (_lb dcls stat _rb)
          (declare (ignore _lb _rb))
	  (setf (stat-declarations stat)
		(append dcls (stat-declarations stat)))
	  stat))
   ({ stat-list }
      #'(lambda (_lb stat _rb)
	  (declare (ignore _lb _rb))
	  stat))
   ({ decl-list	}
      #'(lambda (_lb dcls _rb)
	  (declare (ignore _lb _rb))
	  (make-stat :declarations dcls)))
   ({ }
      #'(lambda (_lb _rb)
	  (declare (ignore _lb _rb))
	  (make-stat))))

  (stat-list
   stat
   (stat-list stat
    #'(lambda (st1 st2)
	(merge-stat st1 st2))))

  (selection-stat
   (|if| \( exp \) stat
       #'(lambda (_op _lp exp _rp stat)
	   (declare (ignore _op _lp _rp))
	   (expand-if-statement exp stat)))
   (|if| \( exp \) stat |else| stat
       #'(lambda (_op _lp exp _rp stat1 _el stat2)
	   (declare (ignore _op _lp _rp _el))
	   (expand-if-statement exp stat1 stat2)))
   (|switch| \( exp \) stat
	   #'(lambda (_k _lp exp _rp stat)
	       (declare (ignore _k _lp _rp))
	       (expand-switch exp stat))))

  (iteration-stat
   (|while| \( exp \) stat
	  #'(lambda (_k _lp cond _rp body)
	      (declare (ignore _k _lp _rp))
	      (expand-loop body :cond cond)))
   (|do| stat |while| \( exp \) \;
     #'(lambda (_k1 body _k2 _lp cond _rp _t)
	 (declare (ignore _k1 _k2 _lp _rp _t))
	 (expand-loop body :cond cond :post-test-p t)))
   (|for| \( exp \; exp \; exp \) stat
	#'(lambda (_k _lp init _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :init init :cond cond :step step)))
   (|for| \( exp \; exp \;     \) stat
	#'(lambda (_k _lp init _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :init init :cond cond)))
   (|for| \( exp \;     \; exp \) stat
	#'(lambda (_k _lp init _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :init init :step step)))
   (|for| \( exp \;     \;     \) stat
	#'(lambda (_k _lp init _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :init init)))
   (|for| \(     \; exp \; exp \) stat
	#'(lambda (_k _lp      _t1 cond _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :cond cond :step step)))
   (|for| \(     \; exp \;     \) stat
	#'(lambda (_k _lp      _t1 cond _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :cond cond)))
   (|for| \(     \;     \; exp \) stat
	#'(lambda (_k _lp      _t1      _t2 step _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body :step step)))
   (|for| \(     \;     \;     \) stat
	#'(lambda (_k _lp      _t1      _t2      _rp body)
	    (declare (ignore _k _lp _t1 _t2 _rp))
	    (expand-loop body))))

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
   (exp \, assignment-exp
	(lispify-binary 'progn)))

  ;; 'assignment-operator' is included here
  (assignment-exp
   conditional-exp
   (unary-exp with-c-syntax.syntax:= assignment-exp
              (lispify-binary 'setf))
   (unary-exp *= assignment-exp
	      (lispify-binary 'mulf))
   (unary-exp with-c-syntax.syntax:/= assignment-exp
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
   (logical-or-exp ? exp \: conditional-exp
		   #'(lambda (cnd _op1 then-exp _op2 else-exp)
		       (declare (ignore _op1 _op2))
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
		 (lispify-binary 'eql))
   (equality-exp != relational-exp
		 (lispify-binary 'not-eql)))

  (relational-exp
   shift-expression
   (relational-exp with-c-syntax.syntax:< shift-expression
		   (lispify-binary '<))
   (relational-exp with-c-syntax.syntax:> shift-expression
		   (lispify-binary '>))
   (relational-exp with-c-syntax.syntax:<= shift-expression
		   (lispify-binary '<=))
   (relational-exp with-c-syntax.syntax:>= shift-expression
		   (lispify-binary '>=)))

  (shift-expression
   additive-exp
   (shift-expression << additive-exp
		     (lispify-binary 'ash))
   (shift-expression >> additive-exp
		     (lispify-binary 'reverse-ash)))

  (additive-exp
   mult-exp
   (additive-exp with-c-syntax.syntax:+ mult-exp
		 (lispify-binary '+))
   (additive-exp with-c-syntax.syntax:- mult-exp
		 (lispify-binary '-)))

  (mult-exp
   cast-exp
   (mult-exp with-c-syntax.syntax:* cast-exp
	     (lispify-binary '*))
   (mult-exp with-c-syntax.syntax:/ cast-exp
	     (lispify-binary '/))
   (mult-exp % cast-exp
	     (lispify-binary 'mod)))

  (cast-exp
   unary-exp
   (\( type-name \) cast-exp
       #'(lambda (_lp type _rp exp)
	   (declare (ignore _lp _rp))
           (lispify-cast type exp))))

  ;; 'unary-operator' is included here
  (unary-exp
   postfix-exp
   (with-c-syntax.syntax:++ unary-exp
       (lispify-unary 'incf))
   (-- unary-exp
       (lispify-unary 'decf))
   (& cast-exp
      #'(lambda (_op exp)
          (declare (ignore _op))
	  (lispify-address-of exp)))
   (with-c-syntax.syntax:* cast-exp
      (lispify-unary 'pseudo-pointer-dereference))
   (with-c-syntax.syntax:+ cast-exp
      (lispify-unary '+))
   (with-c-syntax.syntax:- cast-exp
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
                   (let ((array-dim (and (listp tp) (third tp))))
		     (when (or (not array-dim)
                               (member-if-not #'numberp array-dim))
		       (error 'compile-error
                              :format-control "This array dimension is incompleted: ~S."
                              :format-arguments (list tp)))
		     (apply #'* array-dim))
		   1))))

  (postfix-exp
   primary-exp
   (postfix-exp [ exp ]
		#'(lambda (exp _lb idx _rb)
		    (declare (ignore _lb _rb))
                    (if (starts-with 'lisp-subscript exp)
			(add-to-tail exp idx)
                        `(lisp-subscript ,exp ,idx))))
   (postfix-exp \( argument-exp-list \)
		#'(lambda (exp _lp args _rp)
		    (declare (ignore _lp _rp))
                    (lispify-funcall exp args)))
   (postfix-exp \( \)
		#'(lambda (exp _lp _rp)
		    (declare (ignore _lp _rp))
                    (lispify-funcall exp nil)))
   (postfix-exp \. id
		#'(lambda (exp _op id)
		    (declare (ignore _op))
		    `(struct-member ,exp ',id)))
   (postfix-exp -> id
		#'(lambda (exp _op id)
		    (declare (ignore _op))
		    `(struct-member (pseudo-pointer-dereference ,exp) ',id)))
   (postfix-exp with-c-syntax.syntax:++
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
   lisp-expression			; added
   (|__offsetof| \( decl-specs \, id \)   ; added
                 #'(lambda (_op _lp dspecs _cm id _rp)
                     (declare (ignore _op _lp _cm _rp))
                     (lispify-offsetof dspecs id))))


  (argument-exp-list
   (assignment-exp
    #'list)
   (argument-exp-list \, assignment-exp
                      #'concatenate-comma-list))

  (const
   int-const
   char-const
   float-const
   #+ignore enumeration-const)		; currently unused
  )

;;; Macro interface
(defmacro with-c-compilation-unit ((entry-form return-last?)
				   &body body)
  "* Syntax
~with-c-compilation-unit~ (entry-form return-last?) &body form* => result*

* Arguments and Values
- entry-form  :: a form
- return-last? :: a boolean
- forms       :: a implicit progn
- results     :: the values returned by forms

* Description
Establishes variable bindings for a new compilation.
"
  `(let ((*struct-specs* (copy-hash-table *struct-specs*))
         (*typedef-names* (copy-hash-table *typedef-names*))
         (*dynamic-binding-requested* nil)
         (*function-pointer-ids* nil)
         (*toplevel-entry-form* ,entry-form)
	 (*return-last-statement* ,return-last?))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-c-syntax (body try-add-{} entry-form return-last?)
    (handler-case
	(with-c-compilation-unit (entry-form return-last?)
	  (parse-with-lexer (list-lexer body) *expression-parser*))
      (yacc-parse-error (condition)
	(if (and try-add-{}
		 (not (starts-with '{ body)))
	    (expand-c-syntax `({ ,@body }) nil entry-form return-last?)	; retry
	    (error 'with-c-syntax-parse-error
		   :yacc-error condition))))))

(defmacro with-c-syntax ((&rest options
			  &key (keyword-case (readtable-case *readtable*))
                          (case-sensitive (ecase keyword-case
                                            ((:upcase :downcase) nil)
                                            ((:preserve :invert) t)))
			  (return :auto)
			  (try-add-{} t))
			 &environment *wcs-expanding-environment*
			 &body body)
  "This macro is a entry point of the with-c-syntax system. BODY will be
interpreted as C syntax, executed, and return values.


CASE-SENSITIVE specifies case-sensitivity in interpreting symbols.
If nil, C syntactic keyworks and Libc functions are treated case-insentisively.

KEYWORD-CASE is used for calculating CASE-SENSITIVE parameter.

If RETURN is `:auto', returns the last form's value if BODY is a
compound statement. (If BODY is a compilation unit, this returns NIL
now, but this behavior may be changed.)

If RETURN is any other value, its valus is inserted after the
compilation result translation units. (This feature is intended to
access 'static' variables.)

?f TRY-ADD-{} is t and an error occurred at parsing, `with-c-syntax'
adds '{' and '}' into the head and tail of FORM respectively, and
tries to parse again."
  (cond
    ((null body)
     nil)
    ((and (length= 1 (the list body))	; with-c-syntax is nested.
	  (starts-with 'with-c-syntax (first body)))
     (destructuring-bind (op_ options2 &body body2)
	 (first body)
       (declare (ignore op_))
       `(with-c-syntax (,@options ,@options2) ,@body2)))
    (t
     (expand-c-syntax (preprocessor body case-sensitive)
		      try-add-{}
		      (if (eq return :auto) nil return)
		      (eq return :auto)))))
