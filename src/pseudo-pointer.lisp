(in-package #:with-c-syntax.core)

(deftype pseudo-pointer (&optional (pointee-type t))
  "* Supertypes
fixnum

* Description
The base type of internal representation of pointers in with-c-syntax.

* Compound Type Specifier Syntax
~psendo-pointer~ &optional (pointee-type t)

* Compound Type Specifier Arguments
- pointee-type :: the type of the object pointer by the pointer.

* Compound Type Specifier Description
This notation includes the type of the pointed object.

* Notes
At this stage, the ~pointee-type~ is ignored by the type system.
"  
  (declare (ignore pointee-type))
  'fixnum)

(defconstant +pseudo-pointer-mask+    #b1111111
  "* Value Type
a fixnum.

* Description:
This constant holds the bitmask splits the pseudo-pointer bit
representation to the 'base part' and the 'index part'.

The 'base part' is used for a key of ~*pseudo-pointee-table*~.
The 'index part' is used for a offset of the pointee value.
")

(defconstant +pseudo-pointer-safebit+ #b1000000
  "* Value Type
a fixnum.

* Description
This constant holds the 'sign bit' of the 'index part' of the
pseudo-pointer bit representation.

For the 'index part', the representation is like here:

- #b1000000 :: 0
- #b1000001 - #b1111111 :: +1 ~ +63
- #b0111111 - #b0000000 :: -1 ~ -64

If the 'index part' exceeds this limitation, its result is unpredictable.
")

(defvar *pseudo-pointee-table* (make-hash-table :test 'eq)
  "* Value Type
a hash-table :: <pseudo-pointer> -> <object>

* Description
This table relates the 'base part' of a pseudo-pointer to the pointee
object.
")

(defvar *pseudo-pointer-next* 0
  "* Value Type
a pseudo-pointer.

* Description
This variable the next allocating pseudo-pointer.
")

(defmacro with-pseudo-pointer-scope (() &body body)
  "* Syntax
~with-pseudo-pointer-scope~ () form* => result*

* Arguments and Values
- forms   :: a implicit progn
- results :: the values returned by the ~forms~

* Description
This macro establishes a new environment for pseudo-pointers.

A pointer allocated inside this macro is invalidated out of this.
"
  `(let ((*pseudo-pointee-table* (copy-hash-table *pseudo-pointee-table*))
         (*pseudo-pointer-next* *pseudo-pointer-next*))
     ,@body))

(defun alloc-pseudo-pointer (pointee)
  (incf *pseudo-pointer-next*)
  (let* ((base (ash *pseudo-pointer-next*
                    (logcount +pseudo-pointer-mask+)))
         (p (+ base +pseudo-pointer-safebit+)))
    (setf (gethash base *pseudo-pointee-table*)
	  pointee)
    p))

(defun pseudo-pointer-extract (p &optional (errorp t))
  (let* ((base (logandc2 p +pseudo-pointer-mask+))
	 (idx (- (logand p +pseudo-pointer-mask+)
		 +pseudo-pointer-safebit+))
	 (obj (gethash base *pseudo-pointee-table*)))
    (unless obj
      (when errorp (error "danglinng pointer ~A" p)))
    (values obj idx base)))

(defun pseudo-pointer-pointee (p)
  (pseudo-pointer-extract p))

(defun pseudo-pointer-dereference (p)
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract p)
    (etypecase obj
      (symbol
       (unless (zerop idx)
	 (error "out of index to symbol-reference (~A)" idx))
       (symbol-value obj))
      (vector
       (aref obj idx))
      (array
       (make-pseudo-pointer
	(make-reduced-dimension-array obj idx))))))

(defun (setf pseudo-pointer-dereference) (val p)
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract p)
    (etypecase obj
      (symbol
       (unless (zerop idx)
	 (error "out of index to symbol-reference (~A)" idx))
       (set obj val))
      (vector
       (setf (elt obj idx) val)))))

(defun make-pseudo-pointer (pointee &optional (initial-offset 0))
  (let ((p (alloc-pseudo-pointer pointee)))
    (+ p initial-offset)))

(defun pseudo-pointer-pointable-p (obj)
  (typecase obj
    (symbol t)
    (vector t)
    (array t)
    (t nil)))

;;; TODO:
;;; 1. Use weak hash-table. Its value may held by a weak pointer.
;;; 2. Defragments.
