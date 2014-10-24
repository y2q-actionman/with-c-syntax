;;;; - struct-spec :: the compile-time respesentation of struct or union.
;;;; - struct :: the runtime respesentation of struct or union.

(in-package #:with-c-syntax.core)

(defstruct struct-spec
  "Represents a struct/union specification."
  struct-name	     ; user supplied struct tag (symbol)
  struct-type	     ; 'struct or 'union
  member-defs	     ; (:lisp-type ... :constness ... :decl-specs ...)
  ;; compile-time only
  (defined-in-this-unit nil)) ; T only when compiling this

(defmethod make-load-form ((sspec struct-spec) &optional environment)
  (make-load-form-saving-slots sspec
   :slot-names '(struct-name struct-type member-defs)
   :environment environment))

(defvar *struct-specs* (make-hash-table :test 'eq)
  "* Value Type
a hashtable :: a symbol -> a list of struct-spec.

* Description
Holds definitions of structs or unions.

* Notes
At the beginning of ~with-c-syntax~, it binds this variable to the
values they held. This behavior limits the scope of local struct/union
definitions into the compilation unit.

* Affected By
~with-c-compilation-unit~.

* See Also
~find-struct-spec~, ~add-struct-spec~, ~remove-struct-spec~
")

(defun find-struct-spec (name)
  "* Syntax
~find-struct-spec~ name => struct-spec

* Arguments and Values
- name      :: a symbol
- decl-spec :: a struct-spec instance, or nil.

* Description
Finds and returns a struct-spec. If no struct-specs are found, returns
nil.

* Affected By
~with-c-compilation-unit~.
"
  (first (gethash name *struct-specs*)))

(defun add-struct-spec (name sspec)
  "* Syntax
~add-struct-spec~ name sspec => struct-spec

* Arguments and Values
- name        :: a symbol.
- sspec       :: a struct-spec instance.
- struct-spec :: a struct-spec instance.

* Description
Establishes a new struct-spec definition named ~name~.

* Affected By
~with-c-compilation-unit~.
"
  (push sspec (gethash name *struct-specs*)))

(defun remove-struct-spec (name)
  "* Syntax
~remove-struct-spec~ name => struct-spec

* Arguments and Values
- name        :: a symbol
- struct-spec :: a struct-specs instance, or nil.

* Description
Removes a struct-spec definition named ~name~.

Returns the removed struct-spec definition. If no struct-specs are
found, returns nil.

* Affected By
~with-c-compilation-unit~.
"
  ;; FIXME: remove these bad behavior:
  ;; - Doesn't remove 'nil' from *typedef-names*.
  ;; - Even if an entry does not exist, 'pop' inserts nil.
  (pop (gethash name *struct-specs*)))


(defclass struct ()
  ((member-index-table :initarg :member-index-table
		       :reader struct-member-index-table)
   (member-vector :initarg :member-vector
		  :initform #()
		  :accessor struct-member-vector))
  (:documentation
  "* Class Precedence List
struct, standard-object, ...

* Description
A representation of C structs or unions.
"))

(defun make-struct (spec-obj &rest init-args)
  "* Syntax
~make-struct~ spec-obj &rest init-args => new-struct

* Arguments and Values
- spec-obj :: a symbol, or an instance of struct-spec.
- init-args :: objects.
- new-struct :: an instance of struct.

* Description
Makes a new struct instance based on the specification of ~spec-obj~.

If ~spec-obj~ is a symbol, it must be a name of a globally defined
struct or union. To define a struct or an union globally, use
~with-c-syntax~ and declare it in a toplevel of translation-unit.

If ~spec-obj~ is an instance of struct-spec, it is used directly.

~init-args~ is used for initializing members of the newly created
instance.  If the number of ~init-args~ is less than the number of
members, the rest members are initialized with the default values
specified by the ~spec-obj~.
"
  (typecase spec-obj
    (struct-spec t)
    (symbol
     (if-let ((sspec (find-struct-spec spec-obj)))
       (setf spec-obj sspec)
       (error 'runtime-error
              :format-control "No struct defined named ~S."
              :format-arguments (list spec-obj))))
    (otherwise
     (error 'runtime-error
            :format-control "Not a valid struct-spec object: ~S."
            :format-arguments (list spec-obj))))
  (loop with union-p = (eq (struct-spec-struct-type spec-obj) '|union|)
     with member-index-table = (make-hash-table :test #'eq)
     for idx from 0
     for init-arg = (pop init-args)
     for member-def in (struct-spec-member-defs spec-obj)
     do (setf (gethash (getf member-def :name) member-index-table)
	      (if union-p 0 idx))
     collect (or init-arg (getf member-def :initform))
     into member-inits
     finally
       (return
	 (make-instance 'struct
			:member-index-table member-index-table
			:member-vector (make-array `(,idx)
						   :initial-contents member-inits)))))

(defun struct-member-index (struct member-name)
  "Returns the index of ~member-name~ in the internal storage of
~struct~."
  (let* ((table (struct-member-index-table struct))
         (index (gethash member-name table)))
    (unless index
      (error 'runtime-error
             :format-control  "The struct member ~S is not found."
             :format-arguments (list member-name)))
    index))

(defun struct-member (struct member-name)
  "* Syntax
~struct-member~ struct member-name => object

* Arguments and Values
- struct :: an instance of struct
- member-name :: a symbol
- object :: an object

* Description
Returns the value of the member named ~member-name~ in the ~struct~.
"
  (let ((idx (struct-member-index struct member-name)))
    (aref (struct-member-vector struct) idx)))

(defun (setf struct-member) (val struct member-name)
  "* Syntax
(setf (~struct-member~ struct member-name) new-object)

* Arguments and Values
- struct :: an instance of struct
- member-name :: a symbol
- new-object :: an object

* Description
Sets the value of the member named ~member-name~ in the ~struct~.
"
  (let ((idx (struct-member-index struct member-name)))
    (setf (aref (struct-member-vector struct) idx) val)))
