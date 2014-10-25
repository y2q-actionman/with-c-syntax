(in-package #:with-c-syntax.core)

(deftype pseudo-pointer (&optional (pointee-type t))
  "* Supertypes
pseudo-pointer, fixnum, ...

* Description
The base type of internal representation of pointers in with-c-syntax.

* Compound Type Specifier Syntax
~pseudo-pointer~ &optional (pointee-type t)

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
Holds the bitmask splits the pseudo-pointer bit representation to the
'base part' and the 'index part'.

The 'base part' is used for a key of ~*pseudo-pointee-table*~.
The 'index part' is used for a offset of the pointee value.
")

(defconstant +pseudo-pointer-safebit+ #b1000000
  "* Value Type
a fixnum.

* Description
Holds the 'sign bit' of the 'index part' of the pseudo-pointer bit
representation.

For the 'index part', the representation is like here:

- #b1000000 :: 0
- #b1000001 - #b1111111 :: +1 ~ +63
- #b0111111 - #b0000000 :: -1 ~ -64

If the 'index part' exceeds this limitation, the result is unpredictable.
")

(defvar *pseudo-pointee-table* (make-hash-table)
  "* Value Type
a hash-table :: <pseudo-pointer> -> <object>

* Description
Holds a table relates the 'base part' of a pseudo-pointer to the
pointee object.
")

(defvar *pseudo-pointer-next* 0
  "* Value Type
a pseudo-pointer.

* Description
Holds the next allocating pseudo-pointer.
")

(defmacro with-pseudo-pointer-scope (() &body body)
  "* Syntax
~with-pseudo-pointer-scope~ () form* => result*

* Arguments and Values
- forms   :: a implicit progn
- results :: the values returned by the ~forms~

* Description
Establishes a new environment for pseudo-pointers.

A pointer made inside this macro is invalidated out of this.
"
  `(let ((*pseudo-pointee-table* (copy-hash-table *pseudo-pointee-table*))
         (*pseudo-pointer-next* *pseudo-pointer-next*))
     ,@body))

(defun invalidate-all-pseudo-pointers ()
  "* Syntax
~invalidate-all-pseudo-pointers~ <no arguments> => count

* Arguments and Values
- count :: an integer.

* Description
Invalidates all pseudo-pointers in the scope it called.
 This call doesn't effect pointers out of the
~with-pseudo-pointer-scope~.

Returns the number of invalidates pointers.

* See Also
~with-pseudo-pointer-scope~.
"
  (prog1 (hash-table-count *pseudo-pointee-table*)
    (clrhash *pseudo-pointee-table*)
    (setf *pseudo-pointer-next* 0)))

(defun pseudo-pointer-pointable-p (obj)
  "* Syntax
~pseudo-pointer-pointable-p~ object => boolean

* Arguments and Values
- object  :: an object.
- boolean :: a boolean.

* Description
Returns whether the ~object~ can be held by pseudo-pointers.

* Notes
At this version, this function returns t only if the ~object~ is a
symbol, vector, or an array.
"
  (typecase obj
    (symbol t)
    (vector t)
    (array t)
    (otherwise nil)))

(defun make-pseudo-pointer (pointee &optional (initial-offset 0))
  "* Syntax
~make-pseudo-pointer~ pointee &optional initial-offset => pointer

* Arguments and Values
- pointee        :: an object.
- initial-offset :: an integer. default is 0.
- pointer        :: a newly allocated pseudo-pointer.

* Description
Makes and returns a new pseudo-pointer points ~pointee~.

~initial-offset~ is added to the ~pointer~ at making.
"
  (unless (pseudo-pointer-pointable-p pointee)
    (error 'pseudo-pointer-type-error :pointee pointee))
  (let ((base (ash (incf *pseudo-pointer-next*)
		   (logcount +pseudo-pointer-mask+))))
    (setf (gethash base *pseudo-pointee-table*)
	  pointee)
    (+ base +pseudo-pointer-safebit+ initial-offset)))

(defun pseudo-pointer-extract (p &optional (errorp t))
  "A helper function used by pseudo-pointer-dereference, etc."
  (let* ((base (logandc2 p +pseudo-pointer-mask+))
	 (idx (- (logand p +pseudo-pointer-mask+)
		 +pseudo-pointer-safebit+))
	 (obj (gethash base *pseudo-pointee-table*)))
    (unless obj
      (when errorp
	(error 'pseudo-pointer-dangling-error
	       :pointer p :pointee obj :offset idx)))
    (values obj idx base)))

(defun pseudo-pointer-dereference (p)
  "* Syntax
~pseudo-pointer-dereference~ pointer => object

* Arguments and Values
- pointer :: a pseudo-pointer.
- object  :: an object.

* Description
Dereferences the ~pointer~ and returns the result.
"
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract p)
    (typecase obj
      (symbol
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer p :pointee obj :offset idx))
       (symbol-value obj))
      (vector
       (aref obj idx))
      (array
       (make-pseudo-pointer
	(make-reduced-dimension-array obj idx)))
      (otherwise
       (error 'pseudo-pointer-type-error :pointee obj)))))

(defun (setf pseudo-pointer-dereference) (val p)
  "* Syntax
 (setf (~pseudo-pointer-dereference~ pointer) new-value)

* Arguments and Values
- pointer   :: a pseudo-pointer.
- new-value :: an object.

* Description
Makes the ~pointer~ to point the ~new-value~ object.
"
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract p)
    (typecase obj
      (symbol
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer p :pointee obj :offset idx))
       (set obj val))
      (vector
       (setf (elt obj idx) val))
      (array
       (error 'pseudo-pointer-write-error
	      :pointer p :pointee obj :offset idx))
      (otherwise
       (error 'pseudo-pointer-type-error :pointee obj)))))

(defun pseudo-pointer-invalidate (p)
  "* Syntax
~pseudo-pointer-invalidate~ pointer => boolean

* Arguments and Values
- pointer  : a pseudo-pointer.
- boolean :: a boolean.

* Description
Makes the ~pointer~ to point no objects.  After that, calling
~pseudo-pointer-dereference~ to this pointer will be error.
"
  (multiple-value-bind (obj idx base)
      (pseudo-pointer-extract p nil)
    (declare (ignore obj idx))
    (remhash base *pseudo-pointee-table*)))

;;; TODO:
;;; 1. Use weak hash-table. Its value may held by a weak pointer.
;;; 2. Defragments.
