(in-package #:with-c-syntax.core)

;;; Variables
(defvar *function-pointer-ids* nil
  "Holds a list of symbols, which are declared as a pointer
to a function.  (Because such a symbol is specially treated by the
function-calling expression.)")

(defun delete-symbols-from-function-pointer-ids (symbols)
  (loop for sym in symbols
        do (deletef *function-pointer-ids*
                    sym :test #'eq :count 1)))

(defvar *return-last-statement* t
  "Specifies whether `with-c-syntax' returns the last form's value of
  compound statements or not.")

(defvar *wcs-expanding-environment* nil
  "`with-c-syntax' bind this to `&environment' argument.")

(defmacro with-c-compilation-unit ((&key return-last-statement)
				   &body body)
  "Establishes variable bindings for a new compilation."
  `(let ((*struct-specs* (copy-hash-table *struct-specs*))
         (*typedef-names* (copy-hash-table *typedef-names*))
	 (*return-last-statement* ,return-last-statement)
         (*function-pointer-ids* nil))
     ,@body))

;;; Declarations
(defstruct decl-specs
  "Represents 'decl-specs' in C syntax BNF."
  ;; Filled by the parser
  (type-spec nil)
  (storage-class nil)
  (qualifier nil)
  ;; Filled by `finalize-decl-specs', and referred by `finalize-init-declarator'
  (lisp-type t)             ; typename for Common Lisp
  (tag nil)		    ; struct/union/enum tag
  (typedef-init-decl nil)   ; typedef
  ;; Filled by `finalize-decl-specs', and referred by `expand-declarator-to-nest-macro-elements'
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
  (lisp-type)
  (function-pointer-p nil)) ; T if this variable is treated as a function pointer.

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

(deftype with-c-syntax-char ()
  "Represents C 'char' type, except having one extension: this includes `cl:character'."
  '(or (signed-byte 8) (unsigned-byte 8) character))

(define-constant +numeric-types-alist+
    '(;; Integers
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
      ((|char|)                         .	with-c-syntax-char)
      ((|char| |signed|)                .	(signed-byte 8))
      ((|char| |unsigned|)              .	(unsigned-byte 8))
      ;; Float
      ((|float|)                        .	single-float)
      ((|float| |short|)                .	short-float)
      ((|double|)                       .	double-float)
      ((|double| |long|)                .	long-float))
  :test 'equal
  :documentation
  "An alist of (list-of-symbols . <lisp-type>), holding relationships
 between notations of C type and Common Lisp types.
 For each entry of alist, its car is sorted alphabetically.")

(define-constant +numeric-type-symbols+
    (reduce #'union +numeric-types-alist+ :key #'car) 
  :test 'equal
  :documentation "A list of symbols representing numeric types.")

(defun finalize-numeric-type-spec (dspecs type-spec-list)
  "Fills the decl-specs object referring the passed numeric TYPE-SPEC-LIST."
  (let* ((numeric-symbols (sort (copy-list type-spec-list) #'string<))
         (n-entry (assoc numeric-symbols
                         +numeric-types-alist+
                         :test #'equal)))
    (unless n-entry
      (error 'compile-error
             :format-control "Invalid numeric type: ~A."
             :format-arguments (list numeric-symbols)))
    (setf (decl-specs-lisp-type dspecs) (cdr n-entry))
    dspecs))

(defun finalize-type-spec (dspecs)
  "A part of `finalize-decl-specs'. This processes type-spec."
  (let* ((tp-list (decl-specs-type-spec dspecs))
         (tp (first tp-list)))
    (case (length tp-list)
      (0
       ;; Extension: uses T if no types are specified
       (setf (decl-specs-lisp-type dspecs) t)
       dspecs)
      (1
       (cond
         ((eq tp '|void|)		; void
          (setf (decl-specs-lisp-type dspecs) nil)
          dspecs)
         ((struct-or-union-spec-p tp)   ; struct / union
	  (finalize-struct-or-union-spec tp dspecs))
         ((enum-spec-p tp)              ; enum
	  (finalize-enum-spec tp dspecs))
         ((listp tp)                    ; lisp type
	  (assert (starts-with '|__lisp_type| tp))
	  (setf (decl-specs-lisp-type dspecs) (second tp))
	  dspecs)
         ((find-typedef tp)             ; typedef name
	  (let ((td-dspecs (find-typedef tp)))
            (setf (decl-specs-lisp-type dspecs)
                  (decl-specs-lisp-type td-dspecs)
                  (decl-specs-tag dspecs)
                  (decl-specs-tag td-dspecs)
                  (decl-specs-typedef-init-decl dspecs)
                  (decl-specs-typedef-init-decl td-dspecs))
            dspecs))
         ((member tp +numeric-type-symbols+) ; numeric types
          (finalize-numeric-type-spec dspecs tp-list))
         (t
          (error 'compile-error
                 :format-control "Invalid decl-spec: ~A."
                 :format-arguments (list tp-list)))))
      (otherwise
       (finalize-numeric-type-spec dspecs tp-list)))))

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
                     ((subtypep 'character aref-type)
                      ;; Special handling for 'char'.
                      ;; This check hits our 'char' definition in `+numeric-types-alist+'.
                      ;; In this case, the compiling code is like below:
                      ;;   char str[] = "foo";
                      'character)       ; Will be (simple-array character *)
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
            (values initializer var-type))
           ((type= var-type 't)
            (values initializer var-type))
           ((subtypep var-type 'number) ; includes enum
            (values (or initializer
                        (coerce 0 var-type)) ; This makes '0' for int, '0d0' for double-float.
                    var-type))
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
      (when (and (subtypep var-type nil)
                 (or var-name var-init))
        (error 'compile-error
               :format-control "A void variable cannot be initialized."))
      (setf (init-declarator-lisp-name init-decl) var-name
	    (init-declarator-lisp-initform init-decl) var-init
	    (init-declarator-lisp-type init-decl) var-type)
      (when (and (subtypep var-type 'pseudo-pointer)
                 (starts-with 'pseudo-pointer var-type)
                 (subtypep (second var-type) 'function))
        (setf (init-declarator-function-pointer-p init-decl) t)
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
  (lambda-ignoring-_ (_ exp)
    `(,op ,exp)))

(defun lispify-binary (op)
  (lambda-ignoring-_ (exp1 _ exp2)
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
           (cond
             ((nth-value 1 (macroexpand exp)) ; Checks EXP is a symbol-macro or not.
              `(make-pseudo-pointer-to-place ,exp))
             ;; TODO: Checks whether EXP is a special variable, which can be pointed directly.
             ;; (To check it,  I require `variable-information' of CLtL2.)
             (t
              ;; I assume EXP can be referred without side-effects.
              `(if (pseudo-pointer-pointable-p ,exp)
                   (make-pseudo-pointer ,exp :errorp nil)
                   (make-pseudo-pointer-to-place ,exp)))))
          ((listp exp)
           (destructuring-case exp
             ((lisp-subscript obj &rest args)
              (once-only (obj)
                `(if (arrayp ,obj)
                     (make-pseudo-pointer
                      (make-reduced-dimension-array ,obj ,@(butlast args))
                      :initial-offset ,(lastcar args))
                     (error 'runtime-error
                            :format-control "Trying to get a pointer to an array, but this is not an array: ~S."
                            :format-arguments (list ,obj)))))
             ((struct-member obj mem)
              (once-only (obj)
                `(if (typep ,obj 'struct)
                     (make-pseudo-pointer
                      (struct-member-vector ,obj)
                      :initial-offset (struct-member-index ,obj ,mem))
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

(defun merge-stat (s1 s2)
  (make-stat :code (append (stat-code s1)
			   (stat-code s2))
	     :declarations (append (stat-declarations s1)
				   (stat-declarations s2))
	     :break-statements (append (stat-break-statements s1)
				       (stat-break-statements s2))
	     :continue-statements (append (stat-continue-statements s1)
					  (stat-continue-statements s2))
	     :case-label-list (append (stat-case-label-list s1)
				      (stat-case-label-list s2))))

(defun expand-if-statement-into-stat (exp then-stat &optional (else-stat nil))
  (let* ((stat (if else-stat
		   (merge-stat then-stat else-stat)
		   then-stat))
	 (else-tag (gensym "if-else-"))
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
	       :break-statements (list ret)))) ; Shares '(go <>)' form with above `:code' slot..

(defun rewrite-break-statements (sym stat)
  (let ((ret (shiftf (stat-break-statements stat) nil)))
    (dolist (i ret ret)
      ;; Because `:break-statements' shares '(go <>)' with `:code',
      ;; rewriting the cdr of '(go <>)' will affects `:code'.
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

(defun expand-loop-into-stat (body-stat &key (init nil) (cond t) (step nil)
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

(defun expand-switch-into-stat (exp stat)
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

;;; Function definitions

(defstruct (param-decl (:constructor %make-param-decl))
  "Represents 'param-decl' in C syntax BNF."
  decl-specs
  ;; In BNF, param-decl takes 'declarator', not 'init-declarator'.
  ;; I however use the init-declarator object here, for utilizing
  ;; `finalize-init-declarator' function which fills `lisp-type' slot.
  init-declarator)

(defun make-param-decl (&key (decl-specs (make-decl-specs)) (declarator nil))
  (finalize-decl-specs decl-specs)
  (let ((init-declarator (make-init-declarator :declarator declarator)))
    (finalize-init-declarator decl-specs init-declarator)
    ;; In function parameter, function name is a function pointer.
    (when (subtypep (init-declarator-lisp-type init-declarator) 'function)
      (setf (init-declarator-function-pointer-p init-declarator) t)
      (push (init-declarator-lisp-name init-declarator) *function-pointer-ids*))
    (%make-param-decl :decl-specs decl-specs
                      :init-declarator init-declarator)))

(defmethod make-load-form ((obj param-decl) &optional environment)
  (make-load-form-saving-slots obj
   :slot-names '(decl-specs init-declarator)
   :environment environment))

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

This is not intended for calling directly. The va_start macro uses this."
  (declare (ignore last-argument-name))
  '(error 'runtime-error
    :format-control "Trying to get a variadic args list out of a variadic func."))

(defun function-parameter-void-p (func-param)
  "Checks the function parameter list is '(void)' or not."
  (when-let* ((_ (length= 1 func-param))
              (param1 (first func-param))
              (_ (param-decl-p param1))
              (dspecs (param-decl-decl-specs param1)))
    (equal (decl-specs-type-spec dspecs) '(|void|))))

(defstruct lisp-type-declaration-table
  "Used for generating '(declare type ...)' from C declarators."
  (table (make-hash-table :test 'equal)))

(defun c-type-to-lisp-type-declaration-hack (type)
  "Treats some special type handlings for valid C code."
  (if (and (subtypep type 'pseudo-pointer)
           (listp type))
      (case (second type)
        (function
         ;; Special handing for function-pointers, like:
         ;;   int (*funcptr)(int, int) = func;
         (setf type `(or function ,type)))
        ((with-c-syntax-char)
         ;; Special handing for C string, like:
         ;;   char* str = "Hello, World!;
         (setf type `(or string ,type)))
        (otherwise
         type))
      type))

(defun push-lisp-type-declaration-table (lisp-type-declaration-table type name)
  (unless (eq type t)
    (let ((table (lisp-type-declaration-table-table lisp-type-declaration-table))
          (type (c-type-to-lisp-type-declaration-hack type)))
      (push name (gethash type table)))))

(defun merge-lisp-type-declaration-table (lisp-type-declaration-table1 lisp-type-declaration-table2)
  (loop
    with table1 = (lisp-type-declaration-table-table lisp-type-declaration-table1)
    with table2 = (lisp-type-declaration-table-table lisp-type-declaration-table2)
    for type being the hash-key of table2 using (hash-value name-list)
    do (appendf (gethash type table1) name-list))
  lisp-type-declaration-table1)

(defun generate-lisp-type-declarations (lisp-type-declaration-table)
  (loop
    with table = (lisp-type-declaration-table-table lisp-type-declaration-table)
    for type being the hash-key of table using (hash-value name-list)
    collect `(type ,type ,@name-list)))

(defun parse-function-parameter-list (func-param)
  "Parses FUNC-PARAM as a list of `param-decl' structure (C function
 parameter list), returns 5 values.
   1. A list of symbols naming the parameter.
   2. A list of types representing the types of parameters.
   3. A symbol naming the variadic argument if requested.
   4. A list of symbols should be `ignore'd.
   5. A `lisp-type-declaration-table' object."
  (let ((param-ids nil)
        (param-types nil)
        (varargs-sym nil)
        (omitted nil)
        (lisp-type-declaration-table (make-lisp-type-declaration-table))
        (funcptr-syms nil))
    (cond
      ((null func-param)
       (warn 'with-c-syntax-style-warning
             :message "Empty function parameter '()' was compiled to a varidic function, but there is no way to get the arguments.")
       (setf varargs-sym (gensym "empty-parameter-varargs-"))
       (push varargs-sym omitted))
      ((function-parameter-void-p func-param)
       nil)
      (t
       (loop
         for p in func-param
         when (eq p '|...|)
         do (setf varargs-sym (gensym "varargs-"))
            (loop-finish)
         else
         do (let* ((init-decl (param-decl-init-declarator p))
                   (var-name (or (init-declarator-lisp-name init-decl)
                                 (let ((var (gensym "omitted-arg-")))
		                   (push var omitted)
                                   var)))
                   (var-type (init-declarator-lisp-type init-decl)))
              (push var-name param-ids)
              (push var-type param-types)
              (push-lisp-type-declaration-table
               lisp-type-declaration-table var-type var-name)
              (when (init-declarator-function-pointer-p init-decl)
                (push var-name funcptr-syms))))))
    (when varargs-sym
      (nreconcf param-types '(&rest t))
      (push-lisp-type-declaration-table
       lisp-type-declaration-table 'list varargs-sym))
    ;; Drop parameter name from *function-pointer-ids*.
    (delete-symbols-from-function-pointer-ids funcptr-syms)
    (values (nreverse param-ids)
            (nreverse param-types)
            varargs-sym
            omitted
            lisp-type-declaration-table)))

(defun compute-function-return-type (name return-decl-specs)
  (assert (and (symbolp (first name))
               (starts-with :funcall (second name))))
  (let* ((tmp-declarator (list* nil (nthcdr 2 name)))
         (return-type
           (cond
             ((equal tmp-declarator '(nil))
              ;; Simple case -- int func ();
              (decl-specs-lisp-type return-decl-specs))
             (t
              ;; Returning a pointer etc. -- void* func();
              (let ((tmp-init-declarator
                      (make-init-declarator :declarator tmp-declarator)))
                (finalize-init-declarator return-decl-specs tmp-init-declarator)
                (init-declarator-lisp-type tmp-init-declarator))))))
    (cond
      ((eq return-type nil)
       ;; This is a function returns 'void', like: void func() { return; }
       ;; 
       ;; I uses `CL:NIL' type for 'void' because their semantics are
       ;; same.  But with-c-syntax compiles 'return;' to (CL:RETURN (VALUES)).
       ;; It means returns `(VALUES)'.
       '(values))
      (t
       (c-type-to-lisp-type-declaration-hack return-type)))))

(defun lispify-function-definition (name body-stat
                                    &key K&R-decls (return (make-decl-specs)))
  (finalize-decl-specs return)
  (expand-compound-stat-into-stat body-stat nil)
  (let* ((func-name (first name))
         (func-param (getf (second name) :funcall))
	 (storage-class
	   (case (decl-specs-storage-class return)
	     (|static| '|static|)
	     ((nil) '|global|)
	     (otherwise
              (error 'compile-error
                     :format-control "Cannot define a function of storage-class: ~S."
                     :format-arguments (list (decl-specs-storage-class return)))))))
    (multiple-value-bind (param-ids param-types varargs-sym omitted lisp-type-declaration-table)
        (parse-function-parameter-list func-param)
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
      (let* ((return-type (compute-function-return-type name return))
             (lisp-function-type
               `(function (,@param-types) ; Not affected by K&R decls.
                          ,return-type))
             (lisp-type-declarations    ; TODO: Apply K&R decl types.
               (generate-lisp-type-declarations lisp-type-declaration-table))
             (code (stat-code body-stat)))
        (make-function-definition
         :func-name func-name
         :storage-class storage-class
         :func-args `(,@param-ids ,@(if varargs-sym `(&rest ,varargs-sym)))
         :func-body
         `(,@(if omitted `((declare (ignore ,@omitted))))
           ,@(if lisp-type-declarations
                 `((declare ,@lisp-type-declarations)))
           ,(if (and varargs-sym
                     (not (member varargs-sym omitted))) ; If the parameter list is empty..
                `(macrolet ((get-variadic-arguments (&optional (last-argument-name nil l-supplied-p))
                              "locally established `get-variadic-arguments' macro."
			      (when l-supplied-p
			        (unless (eql last-argument-name ',(lastcar param-ids))
				  (warn "~A's last argument before '...' is ~A, but ~A was passed."
				        ',func-name ',(lastcar param-ids) last-argument-name)))
			      ',varargs-sym))
                   (block nil ,@code))
                `(block nil ,@code)))
         :lisp-type lisp-function-type)))))

;;; Compound statement
(defun expand-init-decls (mode decl-specs init-decls)
  "A part of `expand-declarator-to-nest-macro-elements'."
  (let ((storage-class
          (or (decl-specs-storage-class decl-specs)
	      (ecase mode
		(:statement '|auto|)
		(:translation-unit '|global|))))
        (qualifiers (decl-specs-qualifier decl-specs)))
    ;; Check preconditions.
    (when (member '|volatile| qualifiers)
      (warn 'with-c-syntax-style-warning
	    :message "'volatile' qualifier is currently ignored"))
    (case storage-class
      (|auto|
       (when (eq mode :translation-unit)
         (error 'compile-error
                :format-control "At top level, 'auto' variables are not accepted (~S)."
                :format-arguments (list init-decls))))
      (|register|
       (when (eq mode :translation-unit)
         (error 'compile-error
                :format-control "At top level, 'register' variables are not accepted (~S)."
                :format-arguments (list init-decls))))
      (|global|
       (when (eq mode :statement)
         (error 'compile-error
                :format-control "In internal scope, no global vars cannot be defined (~S)."
                :format-arguments (list init-decls)))))
    ;; Main loop.
    (loop
      with lexical-binds = nil
      with dynamic-extent-vars = nil
      with special-vars = nil
      with ignored-names = nil
      with typedef-names = nil
      with funcptr-syms = nil
      with sym-macro-defs = nil
      with toplevel-defs = nil
      with types-table = (make-lisp-type-declaration-table)

      for i in init-decls
      as name = (init-declarator-lisp-name i)
      as init = (init-declarator-lisp-initform i)
      as type = (init-declarator-lisp-type i)

      ;; function declarations
      if (subtypep type 'function)
        do (unless (or (null init) (zerop init))
             (error 'compile-error
                    :format-control "A function cannot have initializer (~S = ~S)."
                    :format-arguments (list name init)))
      else do
        ;; variables
        (when (init-declarator-function-pointer-p i)
          (push name funcptr-syms))
        (ecase storage-class
          (|auto|
           (assert (eq mode :statement))
           (push `(,name ,init) lexical-binds)
           (push-lisp-type-declaration-table types-table type name))
          (|register|
           (assert (eq mode :statement))
           (push `(,name ,init) lexical-binds)
           (push-lisp-type-declaration-table types-table type name)
           ;; TODO: add warnings if a pointer is taken for this variable.
           (push name dynamic-extent-vars))
          (|extern|
           (unless (or (null init) (zerop init))
             (error 'compile-error
                    :format-control "An 'extern' variable cannot have initializer (~S = ~S)."
                    :format-arguments (list name init))))
          (|global|
           (assert (eq mode :translation-unit))
           (push `(progn (defvar ,name)
                         (setf (documentation ',name 'variable)
		               "generated by with-c-syntax for a global variable.")
                         ,@(if (not (eq type t))
                               `((declaim (type ,type ,name)))))
                 toplevel-defs)
	   ;; Temporary use a lexical variable for initializing the
           ;; global variable with static const variables, which are
           ;; lexically bound.
           (let ((init-tmp (gensym (format nil "global-var-~A-tmp-" name))))
             (push `(,init-tmp (setf ,name ,init)) lexical-binds)
             (push init-tmp ignored-names))
           (push name special-vars))
          (|static|
	   (ecase mode
	     (:statement
              ;; To make initialization only once, use a cons made by
              ;; `load-time-value' for a storage.
              ;; (In old code, I tried to use `defvar' for that.
              ;;  See https://github.com/y2q-actionman/with-c-syntax/blob/4a2e437ac500ce0dbd5c00dfe4c13b9a8cfd13b4/src/compiler.lisp#L919-L932 )
	      (let ((st-sym (gensym (format nil "static-var-~A-storage-" name))))
                (push `(,st-sym (load-time-value (cons ,init nil))) lexical-binds)
                (push-lisp-type-declaration-table types-table `(cons ,type null) st-sym)
                (push `(,name (car ,st-sym)) sym-macro-defs)))
	     (:translation-unit
	      ;; lexically bound
	      (push `(,name ,init) lexical-binds)
              (push-lisp-type-declaration-table types-table type name))))
          (|typedef|
           (push name typedef-names)
	   (when (eq mode :translation-unit)
	     (push `(add-typedef ',name ,(find-typedef name))
		   toplevel-defs))))
      finally
         (return
           (values (nreverse lexical-binds)
                   (nreverse dynamic-extent-vars)
                   (nreverse special-vars)
                   (nreverse ignored-names)
		   (nreverse sym-macro-defs)
                   (nreverse typedef-names)
                   (nreverse funcptr-syms)
		   (nreverse toplevel-defs)
                   types-table)))))

(defun expand-declarator-bindings (lexical-binds dynamic-extent-vars
                                   ignored-names special-vars type-decls)
  "Makes a (let* (...) (declare ...)) form except trying to simplify the expansion."
  `(,@(cond (lexical-binds
             `(let* (,@lexical-binds)))
            ((and (not lexical-binds)
                  (or dynamic-extent-vars ignored-names special-vars))
             '(locally))
            (t
             '(progn)))
    ,@(if (or dynamic-extent-vars ignored-names special-vars type-decls)
          `((declare
             ,@(if dynamic-extent-vars
                   `((dynamic-extent ,@dynamic-extent-vars)))
             ,@(if ignored-names
                   `((ignore ,@ignored-names)))
             ,@(if special-vars
                   `((special ,@special-vars)))
             ,@type-decls)))))

(defun expand-declarator-to-nest-macro-elements (mode decls)
  "Expand a list of declarators to a `nest' macro element.
MODE is one of `:statement' or `:translation-unit'"
  (let (;; used for :statement
        lexical-binds
        dynamic-extent-vars
	;; used for :translation-unit
        special-vars
        ignored-names
	;; used for both
	sym-macro-defs
        cleanup-typedef-names
        cleanup-funcptr-syms
        cleanup-struct-specs
	toplevel-defs
        (types-table (make-lisp-type-declaration-table)))
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
                special-vars-1 ignored-names-1 sym-macro-defs-1
                typedef-names-1 funcptr-syms-1
		toplevel-defs-1
                types-table-1)
             (expand-init-decls mode dspecs init-decls)
	   (assert (subsetp dynamic-extent-vars-1 lexical-binds-1
			    :test (lambda (dv lv)
				    (eql dv (car lv)))))
           (nreconcf lexical-binds lexical-binds-1)
           (nreconcf dynamic-extent-vars dynamic-extent-vars-1)
           (nreconcf special-vars special-vars-1)
           (nreconcf ignored-names ignored-names-1)
           (nreconcf sym-macro-defs sym-macro-defs-1)
           (nreconcf cleanup-typedef-names typedef-names-1)
           (nreconcf cleanup-funcptr-syms funcptr-syms-1)
	   (nreconcf toplevel-defs toplevel-defs-1)
           (setf types-table (merge-lisp-type-declaration-table types-table types-table-1))))
    ;; Finally, constructs a compiled form.
    (nreversef lexical-binds)
    (nreversef dynamic-extent-vars)
    (nreversef special-vars)
    (nreversef ignored-names)
    (nreversef sym-macro-defs)
    (nreversef cleanup-typedef-names)
    (nreversef cleanup-funcptr-syms)
    (nreversef cleanup-struct-specs)
    (nreversef toplevel-defs)
    (prog1
        `(,@(if toplevel-defs
                `((progn ,@toplevel-defs)))
          ,(expand-declarator-bindings lexical-binds dynamic-extent-vars
                                       ignored-names special-vars
                                       (generate-lisp-type-declarations types-table))
          ,@(if sym-macro-defs
                `((symbol-macrolet (,@sym-macro-defs)))))
      ;; drop expanded definitions
      (loop for sym in cleanup-typedef-names
         do (remove-typedef sym))
      (loop for c in cleanup-struct-specs
         do (remove-struct-spec (struct-spec-struct-name c)))
      ;; drop symbols specially treated in this unit.
      (delete-symbols-from-function-pointer-ids cleanup-funcptr-syms))))

(defun expand-compound-stat-into-stat (stat return-last-statement)
  (let* ((stat-codes (stat-code stat))
	 (last-form (lastcar stat-codes))
	 (ex-last-code
	   (if (and return-last-statement
		    (typecase last-form
		      (symbol
		       ;; uninterned symbols (gensym) are assumed as C labels.
		       (symbol-package last-form))
		      (list
		       ;; Don't wrap form if `return' already exists.
		       (not (starts-with 'return last-form)))
		      (otherwise t)))
	       `(return ,last-form)
	       last-form))
	 (ex-code
           `(tagbody
               ;; TODO: setjmp() traversal support here?
	       ,@(butlast stat-codes)
	       ,ex-last-code))
         (decl-expands
           (expand-declarator-to-nest-macro-elements :statement (stat-declarations stat)))
         (new-code `(nest ,@decl-expands ,ex-code)))
    (setf (stat-code stat) `(,new-code)
          (stat-declarations stat) nil)
    stat))

(defun expand-lisp-with-stat-into-stat (identifier lisp-expression stat)
  "Handles with-c-syntax extension syntax for 'with-' like macros.
  Expands \" id lisp-expression statement \" to `(,id ,@lisp-expression ,@statement)

  Example:
   with-output-to-string ((*standard-output*)) { princ \( \"Hello, World!\" \) \; } \;
   => (with-output-to-string (*standard-output*) (princ \"Hello, World!\"))

  Note: This loose expansion causes weird but interesting results.
    See test codes in test/stmt.lisp"
  (let* ((code (stat-code stat))
         (new-code `(,identifier ,@lisp-expression ,@code)))
    (setf (stat-code stat) `(,new-code))
    stat))

(defun expand-statement-expression (stat)
  "Expands GCC's statement expression, not in C90.
 See https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html"
  ;; This is almost same with `expand-toplevel-stat'.
  (let ((stat (expand-compound-stat-into-stat stat t)))
    `(block nil ,@(stat-code stat))))

;;; Toplevel
(defun expand-toplevel-stat (stat)
  (let ((code (stat-code stat)))
    ;; If STAT was not already expanded by `expand-compound-stat-into-stat'..
    (unless (and (length= 1 code)
                 (starts-with 'nest (first code)))
      (expand-compound-stat-into-stat stat t)
      (setf code (stat-code stat)))
    `(block nil ,@(stat-code stat))))

(defun expand-toplevel-const-exp (exp)
  `(progn ,exp))

(defun expand-global-function-definition-to-nest-macro-element (fdef-list)
  "Used by `expand-translation-unit'"
  (loop for fdef in fdef-list
        collect `(defun ,(function-definition-func-name fdef)
                     ,(function-definition-func-args fdef)
                   ,@(function-definition-func-body fdef))
        into defun-list
        collect `(declaim (ftype ,(function-definition-lisp-type fdef)
                              ,(function-definition-func-name fdef)))
        into defun-list
        finally
           (return `(progn ,@defun-list))))

(defun expand-static-function-definition-to-nest-macro-element (fdef-list)
  "Used by `expand-translation-unit'"
  (loop for fdef in fdef-list
        collect `(,(function-definition-func-name fdef)
                  ,(function-definition-func-args fdef)
                  ,@(function-definition-func-body fdef))
        into local-funcs
        collect `(ftype ,(function-definition-lisp-type fdef)
                     ,(function-definition-func-name fdef))
        into local-func-decls
        finally
           (return `(labels (,@local-funcs)
                      (declare ,@local-func-decls)))))

(defun %expand-translation-unit-splitter-key (unit)
  "Used by `expand-translation-unit'"
  (cond
    ((function-definition-p unit)
     (function-definition-storage-class unit))
    (t
     nil)))

(defun expand-translation-unit (units)
  ;; Makes a `nest' macro form.
  (loop
    for unit-seg in (split-to-consecutive-parts units :key #'%expand-translation-unit-splitter-key)
    as first-unit = (first unit-seg)
    if (function-definition-p first-unit)
    collect
       (ecase (function-definition-storage-class first-unit)
         (|global|
          (expand-global-function-definition-to-nest-macro-element unit-seg))
         (|static|
          (expand-static-function-definition-to-nest-macro-element unit-seg)))
    into expansions
    else
    append
       (expand-declarator-to-nest-macro-elements :translation-unit unit-seg)
    into expansions
    finally (return `(nest ,@expansions))))

;;; The parser
(define-parser *c-parser*
  (:muffle-conflicts t)         ; for 'dangling else'.
  ;; http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf
  (:terminals
   #.(append (loop for i being the external-symbol of '#:with-c-syntax.syntax
		  collect i)
	     '(id typedef-id
	       int-const char-const float-const
	       string lisp-expression
               with-c-syntax.punctuator:|__pp_const_exp|)))
  (:start-symbol wcs-entry-point)

  ;; Our entry point.
  ;; top level forms in C, or statements
  (wcs-entry-point
   (translation-unit
    ;; I require `lambda' for avoiding `eval-when' around `expand-translation-unit'
    (lambda (us) (expand-translation-unit us)))
   (compound-stat
    (lambda (st) (expand-toplevel-stat st)))
   (with-c-syntax.punctuator:|__pp_const_exp| const-exp ; For preprocessor.
     (lambda-ignoring-_ (_kwd exp) (expand-toplevel-const-exp exp))))


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
    (lambda (ret name k&r-decls body)
      (lispify-function-definition name body
				   :return ret
				   :K&R-decls k&r-decls)))
   (           declarator decl-list compound-stat
    (lambda (name k&r-decls body)
      (lispify-function-definition name body
				   :K&R-decls k&r-decls)))
   (decl-specs declarator           compound-stat
    (lambda (ret name body)
      (lispify-function-definition name body
				   :return ret)))
   (           declarator           compound-stat
    (lambda (name body)
      (lispify-function-definition name body))))

  (decl
   (decl-specs init-declarator-list \;
               (lambda-ignoring-_ (dspecs inits _t)
                 (finalize-decl-specs dspecs)
		 `(,dspecs
		   ,(mapcar (lambda (i) (finalize-init-declarator dspecs i))
                            inits))))
   (decl-specs \;
               (lambda-ignoring-_ (dspecs _t)
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
                       (lambda (cls dspecs)
                         (push cls (decl-specs-storage-class dspecs))
                         dspecs))
   (storage-class-spec
    (lambda (cls)
      (make-decl-specs :storage-class `(,cls))))
   (type-spec decl-specs
              (lambda (tp dspecs)
                (push tp (decl-specs-type-spec dspecs))
                dspecs))
   (type-spec
    (lambda (tp)
      (make-decl-specs :type-spec `(,tp))))
   (type-qualifier decl-specs
                   (lambda (qlr dspecs)
                     (push qlr (decl-specs-qualifier dspecs))
                     dspecs))
   (type-qualifier
    (lambda (qlr)
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
                    (lambda-ignoring-_ (kwd id _l decl _r)
		      (make-struct-or-union-spec
		       :type kwd :id id :struct-decl-list decl)))
   (struct-or-union    { struct-decl-list }
                    (lambda-ignoring-_ (kwd _l decl _r)
		      (make-struct-or-union-spec
		       :type kwd :struct-decl-list decl)))
   (struct-or-union id
                    (lambda (kwd id)
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
    (lambda (d)
      (make-init-declarator :declarator d)))
   (declarator with-c-syntax.syntax:= initializer
               (lambda-ignoring-_ (d _op i)
		 (make-init-declarator :declarator d
				       :initializer i))))

  ;; returns (spec-qualifier-list . struct-declarator-list)
  (struct-decl
   (spec-qualifier-list struct-declarator-list \;
                        (lambda-ignoring-_ (qls dcls _t)
			  (cons qls dcls))))

  ;; returns a spec-qualifier-list structure
  (spec-qualifier-list
   (type-spec spec-qualifier-list
	      (lambda (tp lis)
		(push tp (spec-qualifier-list-type-spec lis))
		lis))
   (type-spec
    (lambda (tp)
      (make-spec-qualifier-list :type-spec `(,tp))))
   (type-qualifier spec-qualifier-list
		   (lambda (ql lis)
		     (push ql (spec-qualifier-list-qualifier lis))
		     lis))
   (type-qualifier
    (lambda (ql)
      (make-spec-qualifier-list :qualifier `(,ql)))))

  (struct-declarator-list
   (struct-declarator
    #'list)
   (struct-declarator-list \, struct-declarator
			   #'concatenate-comma-list))

  ;; returns a struct-declarator structure
  (struct-declarator
   (declarator
    (lambda (d)
      (make-struct-declarator :declarator d)))
   (declarator \: const-exp
	       (lambda-ignoring-_ (d _c bits)
		 (make-struct-declarator :declarator d :bits bits)))
   (\: const-exp
       (lambda-ignoring-_ (_c bits)
	 (make-struct-declarator :bits bits))))

  ;; returns an enum-spec structure
  (enum-spec
   (|enum| id { enumerator-list }
           (lambda-ignoring-_ (_kwd id _l lis _r)
	     (make-enum-spec :id id :enumerator-list lis)))
   (|enum|    { enumerator-list }
           (lambda-ignoring-_ (_kwd _l lis _r)
	     (make-enum-spec :enumerator-list lis)))
   (|enum| id
           (lambda-ignoring-_ (_kwd id)
	     (make-enum-spec :id id))))

  (enumerator-list
   (enumerator
    #'list)
   (enumerator-list \, enumerator
                    #'concatenate-comma-list))

  ;; returns an enumerator structure
  (enumerator
   (id
    (lambda (id)
      (make-enumerator :declarator id)))
   (id with-c-syntax.syntax:= const-exp
       (lambda-ignoring-_ (id _op exp)
         (make-enumerator :declarator id :initializer exp))))

  ;; returns like:
  ;; (name (:aref nil) (:funcall nil) (:aref 5) (:funcall int))
  ;; processed in `expand-init-declarator-init'
  (declarator
   (pointer direct-declarator
            (lambda (ptr dcls)
              (append dcls ptr)))
   direct-declarator)

  (direct-declarator
   (id
    #'list)
   (\( declarator \)
    (lambda-ignoring-_ (_lp dcl _rp)
      dcl))
   (direct-declarator [ const-exp ]
    (lambda-ignoring-_ (dcl _lp params _rp)
      (add-to-tail dcl `(:aref ,params))))
   (direct-declarator [		  ]
    (lambda-ignoring-_ (dcl _lp _rp)
      (add-to-tail dcl '(:aref nil))))
   (direct-declarator \( param-type-list \)
    (lambda-ignoring-_ (dcl _lp params _rp)
      (add-to-tail dcl `(:funcall ,params))))
   (direct-declarator \( id-list \)
    (lambda-ignoring-_ (dcl _lp id-list _rp)
      (add-to-tail dcl `(:funcall
                         ,(mapcar (lambda (id)
                                    (make-param-decl :declarator (list id)))
                                  id-list)))))
   (direct-declarator \(	 \)
    (lambda-ignoring-_ (dcl _lp _rp)
      (add-to-tail dcl '(:funcall nil)))))

  (pointer
   (with-c-syntax.syntax:* type-qualifier-list
    (lambda-ignoring-_ (_kwd qls)
      `((:pointer ,@qls))))
   (with-c-syntax.syntax:*
    (lambda-ignoring-_ (_kwd)
      '((:pointer))))
   (with-c-syntax.syntax:* type-qualifier-list pointer
    (lambda-ignoring-_ (_kwd qls ptr)
      (add-to-tail ptr `(:pointer ,@qls))))
   (with-c-syntax.syntax:*			pointer
    (lambda-ignoring-_ (_kwd ptr)
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
    (lambda (decl-specs declarator)
      (make-param-decl :decl-specs decl-specs :declarator declarator)))
   (decl-specs abstract-declarator
    (lambda (decl-specs declarator)
      (make-param-decl :decl-specs decl-specs :declarator declarator)))
   (decl-specs
    (lambda (decl-specs)
      (make-param-decl :decl-specs decl-specs))))

  (id-list
   (id
    #'list)
   (id-list \, id
    #'concatenate-comma-list))

  (initializer
   assignment-exp
   ({ initializer-list }
    (lambda-ignoring-_ (_lp inits _rp)
      inits))
   ({ initializer-list \, }
    (lambda-ignoring-_ (_lp inits _cm _rp)
      inits)))

  (initializer-list
   (initializer
    #'list)
   (initializer-list \, initializer
    #'concatenate-comma-list))

  ;; see 'decl'
  (type-name
   (spec-qualifier-list abstract-declarator
			(lambda (spec-qual abs)
			  (lispify-type-name spec-qual abs)))
   (spec-qualifier-list
    (lambda (spec-qual)
      (lispify-type-name spec-qual nil))))

  ;; See 'declarator' about abstract-declarator's representation.
  ;; inserts 'nil' as a name
  (abstract-declarator
   (pointer
    (lambda (ptr)
      `(nil ,@ptr)))
   (pointer direct-abstract-declarator
    (lambda (ptr dcls)
      `(nil ,@dcls ,@ptr)))
   (direct-abstract-declarator
    (lambda (adecl)
      `(nil ,@adecl))))

  ;; see 'direct-declarator'
  (direct-abstract-declarator
   (\( abstract-declarator \)
    (lambda-ignoring-_ (_lp dcl _rp)
      dcl))
   (direct-abstract-declarator [ const-exp ]
    (lambda-ignoring-_ (dcls _lp params _rp)
      (add-to-tail dcls `(:aref ,params))))
   (			       [ const-exp ]
    (lambda-ignoring-_ (_lp params _rp)
      `((:aref ,params))))
   (direct-abstract-declarator [	   ]
    (lambda-ignoring-_ (dcls _lp _rp)
      (add-to-tail dcls `(:aref nil))))
   (			       [	   ]
    (lambda-ignoring-_ (_lp _rp)
      '((:aref nil))))
   (direct-abstract-declarator \( param-type-list \)
    (lambda-ignoring-_ (dcls _lp params _rp)
      (add-to-tail dcls `(:funcall ,params))))
   (			       \( param-type-list \)
    (lambda-ignoring-_ (_lp params _rp)
      `((:funcall ,params))))
   (direct-abstract-declarator \(		  \)
    (lambda-ignoring-_ (dcls _lp _rp)
      (add-to-tail dcls '(:funcall nil))))
   (			       \(		  \)
    (lambda-ignoring-_ (_lp _rp)
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
   jump-stat
   lisp-with-stat)                      ; extension

  (labeled-stat
   (id \: stat
       (lambda-ignoring-_ (id _c stat)
	 (push id (stat-code stat))
	 stat))
   (|case| const-exp \: stat
       (lambda-ignoring-_ (_k  exp _c stat)
	 (push-case-label exp stat)
	 stat))
   (|default| \: stat
       (lambda-ignoring-_ (_k _c stat)
	 (push-case-label '|default| stat)
	 stat)))

  (exp-stat
   (exp \;
	(lambda-ignoring-_ (exp _term)
	  (make-stat :code (list exp))))
   (\;
    (lambda-ignoring-_ (_term)
      (make-stat))))

  (compound-stat
   ({ decl-list stat-list }
      (lambda-ignoring-_ (_lb dcls stat _rb)
        (setf (stat-declarations stat)
	      (append dcls (stat-declarations stat)))
        (expand-compound-stat-into-stat stat *return-last-statement*)))
   ({ stat-list }
      (lambda-ignoring-_ (_lb stat _rb)
	stat))
   ({ decl-list	}
      (lambda-ignoring-_ (_lb dcls _rb)
        (expand-compound-stat-into-stat (make-stat :declarations dcls) *return-last-statement*)))
   ({ }
      (lambda-ignoring-_ (_lb _rb)
	(make-stat))))

  (stat-list
   stat
   (stat-list stat
    (lambda (st1 st2)
      (merge-stat st1 st2))))

  (selection-stat
   (|if| \( exp \) stat
         (lambda-ignoring-_ (_op _lp exp _rp stat)
	   (expand-if-statement-into-stat exp stat)))
   (|if| \( exp \) stat |else| stat
         (lambda-ignoring-_ (_op _lp exp _rp stat1 _el stat2)
	   (expand-if-statement-into-stat exp stat1 stat2)))
   (|switch| \( exp \) stat
	     (lambda-ignoring-_ (_k _lp exp _rp stat)
	       (expand-switch-into-stat exp stat))))

  (iteration-stat
   (|while| \( exp \) stat
	    (lambda-ignoring-_ (_k _lp cond _rp stat)
	      (expand-loop-into-stat stat :cond cond)))
   (|do| stat |while| \( exp \) \;
         (lambda-ignoring-_ (_k1 stat _k2 _lp cond _rp _t)
	   (expand-loop-into-stat stat :cond cond :post-test-p t)))
   (|for| \( exp \; exp \; exp \) stat
	  (lambda-ignoring-_ (_k _lp init _t1 cond _t2 step _rp stat)
	    (expand-loop-into-stat stat :init init :cond cond :step step)))
   (|for| \( exp \; exp \;     \) stat
	  (lambda-ignoring-_ (_k _lp init _t1 cond _t2      _rp stat)
	    (expand-loop-into-stat stat :init init :cond cond)))
   (|for| \( exp \;     \; exp \) stat
	  (lambda-ignoring-_ (_k _lp init _t1      _t2 step _rp stat)
	    (expand-loop-into-stat stat :init init :step step)))
   (|for| \( exp \;     \;     \) stat
	  (lambda-ignoring-_ (_k _lp init _t1      _t2      _rp stat)
	    (expand-loop-into-stat stat :init init)))
   (|for| \(     \; exp \; exp \) stat
	  (lambda-ignoring-_ (_k _lp      _t1 cond _t2 step _rp stat)
	    (expand-loop-into-stat stat :cond cond :step step)))
   (|for| \(     \; exp \;     \) stat
	  (lambda-ignoring-_ (_k _lp      _t1 cond _t2      _rp stat)
	    (expand-loop-into-stat stat :cond cond)))
   (|for| \(     \;     \; exp \) stat
	  (lambda-ignoring-_ (_k _lp      _t1      _t2 step _rp stat)
	    (expand-loop-into-stat stat :step step)))
   (|for| \(     \;     \;     \) stat
	  (lambda-ignoring-_ (_k _lp      _t1      _t2      _rp stat)
	    (expand-loop-into-stat stat))))

  (jump-stat
   (|goto| id \;
	   (lambda-ignoring-_ (_k id _t)
	     (make-stat :code (list `(go ,id)))))
   (|continue| \;
	       (lambda-ignoring-_ (_k _t)
		 (make-stat-unresolved-continue)))
   (|break| \;
	    (lambda-ignoring-_ (_k _t)
	      (make-stat-unresolved-break)))
   (|return| exp \;
	     (lambda-ignoring-_ (_k exp _t)
	       (make-stat :code (list `(return ,exp)))))
   (|return| \;
	     (lambda-ignoring-_ (_k _t)
	       (make-stat :code (list `(return (values)))))))

  (lisp-with-stat                       ; extension
   (id lisp-expression stat
       (lambda (id lisp-expression stat)
         (expand-lisp-with-stat-into-stat id lisp-expression stat))))


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
		   (lambda-ignoring-_ (cnd _op1 then-exp _op2 else-exp)
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
       (lambda-ignoring-_ (_lp type _rp exp)
         (lispify-cast type exp))))

  ;; 'unary-operator' is included here
  (unary-exp
   postfix-exp
   (with-c-syntax.syntax:++ unary-exp
       (lispify-unary 'incf))
   (-- unary-exp
       (lispify-unary 'decf))
   (& cast-exp
      (lambda-ignoring-_ (_op exp)
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
	     (lambda-ignoring-_ (_op exp)
	       ;; calculate runtime
	       `(if (arrayp ,exp)
		    (array-total-size ,exp)
		    1)))
   (|sizeof| \( type-name \)
	     (lambda-ignoring-_ (_op _lp tp _rp)
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
		(lambda-ignoring-_ (exp _lb idx _rb)
                  (if (starts-with 'lisp-subscript exp)
		      (add-to-tail exp idx)
                      `(lisp-subscript ,exp ,idx))))
   (postfix-exp \( argument-exp-list \)
		(lambda-ignoring-_ (exp _lp args _rp)
                  (lispify-funcall exp args)))
   (postfix-exp \( \)
		(lambda-ignoring-_ (exp _lp _rp)
                  (lispify-funcall exp nil)))
   (postfix-exp \. id
		(lambda-ignoring-_ (exp _op id)
		  `(struct-member ,exp ',id)))
   (postfix-exp -> id
		(lambda-ignoring-_ (exp _op id)
		  `(struct-member (pseudo-pointer-dereference ,exp) ',id)))
   (postfix-exp with-c-syntax.syntax:++
		(lambda-ignoring-_ (exp _op)
		  `(post-incf ,exp 1)))
   (postfix-exp --
		(lambda-ignoring-_ (exp _op)
		  `(post-incf ,exp -1))))

  (primary-exp
   id
   const
   string
   (\( exp \)
       (lambda-ignoring-_  (_1 x _3)
	 x))
   lisp-expression			; added
   (|__offsetof| \( decl-specs \, id \) ; added
                 (lambda-ignoring-_ (_op _lp dspecs _cm id _rp)
                   (lispify-offsetof dspecs id)))
   (\( stat \)                          ; GCC extension
       (lambda-ignoring-_  (_lp stat _rp)
         (expand-statement-expression stat))))


  (argument-exp-list
   (assignment-exp
    #'list)
   (argument-exp-list \, assignment-exp
                      #'concatenate-comma-list))

  (const
   int-const
   char-const
   float-const
   #+ () enumeration-const)		; currently unused
  )
