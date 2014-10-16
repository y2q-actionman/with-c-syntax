(in-package #:with-c-syntax.core)

;; This file contains the runtime respesentation of 'struct' in
;; with-c-syntax.

;;; struct type
(defclass struct ()
  ((field-index-table :initarg :field-index-table)
   (fields :initarg :fields :initform #()
           :accessor struct-fields))
  (:documentation
  "* Class Precesence List
standard-object

* Description
This class is a representation of C struct or unions.
"))

(defun make-struct (spec-obj &rest init-args)
  "* Syntax
~make-struct~ spec-obj &rest init-args => new-struct

* Arguments and Values
- spec-obj :: a symbol, or an instance of struct-spec.
- init-args :: objects.
- new-struct :: an instance of struct.

* Description
This function makes a new struct instance based on the specification
of ~spec-obj~.

If ~spec-obj~ is a symbol, it must be a name of a globally defined
struct or union. To define a struct or an union globally, use
~with-c-syntax~ and declare it in a toplevel of translation-unit.
 (For hackers: See the usage of ~add-struct-spec~.)

If ~spec-obj~ is an instance of struct-spec, it is used directly.
 (For hackers: See ~expand-init-declarator-init~. This style is used
 in ~with-c-syntax~ internally.)

~init-args~ is used for initializing fields of the newly created
instance.  If the number of ~init-args~ is less than the number of
fields, the rest fields are initialized with the default values
specified by the ~spec-obj~.
"
  (etypecase spec-obj
    (struct-spec t)
    (symbol (setf spec-obj
		  (or (find-struct-spec spec-obj)
		      (error "no struct defined: ~S" spec-obj)))))
  (loop with union-p = (eq (struct-spec-struct-type spec-obj) '|union|)
     with field-index-table = (make-hash-table :test #'eq)
     for idx from 0
     for init-arg = (pop init-args)
     for slot-def in (struct-spec-slot-defs spec-obj)
     do (setf (gethash (getf slot-def :name) field-index-table)
	      (if union-p 0 idx))
     collect (or init-arg (getf slot-def :initform))
     into field-inits
     finally
       (return
	 (make-instance 'struct
			:field-index-table field-index-table
			:fields (make-array `(,idx)
					    :initial-contents field-inits)))))

(defun struct-field-index (struct field-name)
  (let* ((table (slot-value struct 'field-index-table))
         (index (gethash field-name table)))
    (unless index
      (error "field ~S not found" field-name))
    index))

(defun struct-field (struct field-name)
  "* Syntax
~struct-field~ struct field-name => object

* Arguments and Values
- struct :: an instance of struct
- field-name :: a symbol
- object :: an object

* Description
This function returns the value of the field named ~field-name~ in the
~struct~.
"
  (let ((idx (struct-field-index struct field-name)))
    (aref (struct-fields struct) idx)))

(defun (setf struct-field) (val struct field-name)
  "* Syntax
(setf (~struct-field~ struct field-name) new-object)

* Arguments and Values
- struct :: an instance of struct
- field-name :: a symbol
- new-object :: an object

* Description
This setf-function sets the value of the field named ~field-name~ in
the ~struct~.
"
  (let ((idx (struct-field-index struct field-name)))
    (setf (aref (struct-fields struct) idx) val)))
