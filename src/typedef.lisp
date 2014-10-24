;;;; This contains only the repository of defined typedefs.
;;;; Codes implementing the work of typedef are included in
;;;; with-c-syntax.lisp.

(in-package #:with-c-syntax.core)

(defvar *typedef-names* (make-hash-table :test 'eq)
  "* Value Type
a hashtable :: a symbol -> list of decl-specs

* Description
Holds definitions of typedefs.

* Notes
At the beginning of ~with-c-syntax~, it binds this variable to the
values they held. This behavior limits the scope of local typedefs
into the compilation unit.

* Affected By
~with-c-compilation-unit~.

* See Also
~find-typedef~, ~add-typedef~, ~remove-typedef~.
")

(defun find-typedef (name)
  "* Syntax
~find-typedef~ name => decl-spec

* Arguments and Values
- name      :: a symbol
- decl-spec :: a decl-specs instance, or nil.

* Description
Finds and returns a typedef definition. If no typedefs are found,
returns nil.

* Affected By
~with-c-compilation-unit~.
"
  (first (gethash name *typedef-names*)))

(defun add-typedef (name spec)
  "* Syntax
~add-typedef~ name spec => decl-spec

* Arguments and Values
- name      :: a symbol
- spec      :: a decl-specs instance, or a type specifier.
- decl-spec :: a decl-specs instance.

* Description
Establishes a new typedef definition named ~name~.

* Affected By
~with-c-compilation-unit~.
"
  (let ((dspecs
	 (typecase spec
	   (decl-specs spec)
	   (t (make-decl-specs :lisp-type spec)))))
    (push dspecs (gethash name *typedef-names*))
    dspecs))

(defun remove-typedef (name)
  "* Syntax
~remove-typedef~ name => decl-spec

* Arguments and Values
- name      :: a symbol
- decl-spec :: a decl-specs instance, or nil.

* Description
Removes a typedef definition named ~name~.

Returns the removed typedef definition. If no typedefs are found,
returns nil.

* Affected By
~with-c-compilation-unit~.
"
  ;; FIXME: fix bad behaviors like remove-struct-spec. See that.
  (pop (gethash name *typedef-names*)))
