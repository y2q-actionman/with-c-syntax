(in-package #:with-c-syntax.core)

;; This file contains the runtime respesentation of 'struct' in
;; with-c-syntax, named "wcs-struct".

;;; wcs-struct type
(defclass wcs-struct ()
  ((field-index-table :initarg :field-index-table)
   (fields :initarg :fields :initform #()
           :accessor wcs-struct-fields))
  (:documentation
  "* Class Precesence List
standard-object

* Description
This class is a representation of C struct or unions.
"))

(defun make-wcs-struct (spec-obj &rest init-args)
  "* Syntax
~make-wcs-struct~ spec-obj &rest init-args => new-struct

* Arguments and Values
- spec-obj :: a symbol, or an instance of wcs-struct-spec.
- init-args :: objects.
- new-struct :: an instance of wcs-struct.

* Description
This function makes a new wcs-struct instance based on the
specification of ~spec-obj~.

If ~spec-obj~ is a symbol, it must be a name of a globally defined
struct or union. To define a struct or an union globally, use
~with-c-syntax~ and declare it in a toplevel of translation-unit.
 (For hackers: See the usage of ~install-global-wcs-struct-spec~.)

If ~spec-obj~ is an instance of wcs-struct-spec, it is used directly.
This style is used in ~with-c-syntax~.
 (For hackers: See ~expand-init-declarator-init~.)

~init-args~ is used for initializing fields of the newly created
instance.  If the number of ~init-args~ is less than the number of
fields, the rest fields are initialized with the default values
specified by the ~spec-obj~.
"
  (etypecase spec-obj
    (wcs-struct-spec t)
    (symbol (setf spec-obj
		  (or (find-global-wcs-struct-spec spec-obj)
		      (error "no wcs-struct defined: ~S" spec-obj)))))
  (loop with default-args = (wcs-struct-spec-initforms spec-obj)
     with field-index-alist = (wcs-struct-spec-field-index-alist spec-obj)
     with size = (length field-index-alist)
     with init-args-len = (length init-args)
     with ret = (make-instance
                 'wcs-struct
                 :field-index-table (alist-hash-table field-index-alist)
                 :fields (make-array `(,size)))
     for idx from 0 below size
     for (init1 . inits) = init-args then inits
     for (default1 . defaults) = default-args then defaults
     do (setf (aref (wcs-struct-fields ret) idx)
              (if (< idx init-args-len) init1 default1))
     finally (return ret)))

(defun wcs-struct-field-index (wcs-struct field-name)
  (let* ((table (slot-value wcs-struct 'field-index-table))
         (index (gethash field-name table)))
    (unless index
      (error "field ~S not found" field-name))
    index))

(defun wcs-struct-field (wcs-struct field-name)
  "* Syntax
~wcs-struct-field~ wcs-struct field-name => object

* Arguments and Values
- wcs-struct :: an instance of wcs-struct
- field-name :: a symbol
- object :: an object

* Description
This function returns the value of the field named ~field-name~ in the
~wcs-struct~.
"
  (let ((idx (wcs-struct-field-index wcs-struct field-name)))
    (aref (wcs-struct-fields wcs-struct) idx)))

(defun (setf wcs-struct-field) (val wcs-struct field-name)
  "* Syntax
(setf (~wcs-struct-field~ wcs-struct field-name) new-object)

* Arguments and Values
- wcs-struct :: an instance of wcs-struct
- field-name :: a symbol
- new-object :: an object

* Description
This setf-function sets the value of the field named ~field-name~ in
the ~wcs-struct~.
"
  (let ((idx (wcs-struct-field-index wcs-struct field-name)))
    (setf (aref (wcs-struct-fields wcs-struct) idx) val)))
