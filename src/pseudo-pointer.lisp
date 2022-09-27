(in-package #:with-c-syntax.core)

;;; pseudo-pointer type definitions and environments.

(deftype valid-pseudo-pointer (&optional (pointee-type t))
  "The base type of internal representation of non-null pointers in
with-c-syntax system.
Optional POINTEE-TYPE specifies the type of the object pointer by the
pointer."
  ;; (At this stage, the POINTEE-TYPE is ignored by the type system.)
  (declare (ignore pointee-type))
  'fixnum)

(deftype pseudo-pointer (&optional (pointee-type t))
  "The base type of internal representation of null and non-null
pointers in with-c-syntax sysrem."  
  `(or null (valid-pseudo-pointer ,pointee-type)))

(defconstant +pseudo-pointer-mask+    #b1111111
  "A fixnum represents the bitmask splits the pseudo-pointer bit
representation to the 'base part' and the 'index part'.

The 'base part' is used for a key of `*pseudo-pointee-table*'.
The 'index part' is used for a offset of the pointee value.")

(defconstant +pseudo-pointer-safebit+ #b1000000
  "A fixnum specifies the 'sign bit' of the 'index part' of the
pseudo-pointer bit representation.

For the 'index part', the representation is like here:

- #b1000000 :: 0
- #b1000001 - #b1111111 :: +1 ~ +63
- #b0111111 - #b0000000 :: -1 ~ -64

If the 'index part' exceeds this limitation, the result is unpredictable.")

(defvar *pseudo-pointee-table* (make-hash-table)
  "A hash-table holds relations between the 'base part' of a
pseudo-pointer and the pointee object.")

(defvar *next-pseudo-pointer* 0
  "A pseudo-pointer used for the next object.")

(defmacro with-pseudo-pointer-scope (() &body body)
  "In this macro, BODY is executed in a new environment for
pseudo-pointers.
A pointer made inside this macro is invalidated outside of this."
  `(let ((*pseudo-pointee-table* (copy-hash-table *pseudo-pointee-table*))
         (*next-pseudo-pointer* *next-pseudo-pointer*))
     ,@body))

(defun invalidate-all-pseudo-pointers ()
  "This function invalidates all pseudo-pointers in the scope it
called, and returns the number of invalidates pointers.

In `with-pseudo-pointer-scope', this call invalidates pseudo-pointers
made in that.
If out of that, this call invalidates all pseudo-pointers."
  (prog1 (hash-table-count *pseudo-pointee-table*)
    (clrhash *pseudo-pointee-table*)
    (setf *next-pseudo-pointer* 0)))

;;; pseudo-pointer internal types.

(defstruct (pseudo-pointer-to-place
            (:constructor %make-pp-to-place))
  "A pseudo-pointer internal type referring a single place, using `lambda'."
  lambda)

;;; pseudo-pointer interfaces

(defun pseudo-pointer-pointable-p (object)
  "Returns whether the OBJECT can directly be held by pseudo-pointers."
  (typecase object
    (symbol t)
    (vector t)
    (array t)
    (pseudo-pointer-to-place t)
    (otherwise nil)))

(defun make-pseudo-pointer (pointee &key (initial-offset 0) (errorp t))
  "Makes and returns a new pseudo-pointer points POINTEE.
INITIAL-OFFSET is added to the result at making."
  (when (and errorp
             (not (pseudo-pointer-pointable-p pointee)))
    (error 'pseudo-pointer-type-error :pointee pointee))
  (let ((base (ash (incf *next-pseudo-pointer*)
		   (logcount +pseudo-pointer-mask+))))
    (setf (gethash base *pseudo-pointee-table*)
	  pointee)
    (+ base +pseudo-pointer-safebit+ initial-offset)))

(defmacro make-pseudo-pointer-to-place (place)
  "Makes a pseudo-pointer using `pseudo-pointer-to-place'."
  `(make-pseudo-pointer
    (%make-pp-to-place
     :lambda (lambda (mode &optional value)
               (ecase mode
                 (:get ,place)
                 (:set (setf ,place value)))))
    :errorp nil))

(defun pseudo-pointer-extract (pointer &optional (errorp t))
  "A helper function used by `pseudo-pointer-dereference', etc."
  (when (null pointer)
    (if errorp
	(error 'pseudo-pointer-null-dereference-error)
	(return-from pseudo-pointer-extract nil)))
  (let* ((base (logandc2 pointer +pseudo-pointer-mask+))
	 (idx (- (logand pointer +pseudo-pointer-mask+)
		 +pseudo-pointer-safebit+))
	 (obj (gethash base *pseudo-pointee-table*)))
    (unless obj
      (when errorp
	(error 'pseudo-pointer-dangling-error
	       :pointer pointer :pointee obj :offset idx)))
    (values obj idx base)))

(defun pseudo-pointer-dereference (pointer)
  "Dereferences the POINTER and returns the pointed object."
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract pointer)
    (typecase obj
      (symbol
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer pointer :pointee obj :offset idx))
       (symbol-value obj))
      (vector
       (aref obj idx))
      (array
       (make-pseudo-pointer
	(make-reduced-dimension-array obj idx)))
      (pseudo-pointer-to-place
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer pointer :pointee obj :offset idx))
       (funcall (pseudo-pointer-to-place-lambda obj) :get))
      (otherwise
       (error 'pseudo-pointer-type-error :pointee obj)))))

(defun (setf pseudo-pointer-dereference) (new-object pointer)
  "Makes the POINTER to point the NEW-OBJECT."
  (multiple-value-bind (obj idx)
      (pseudo-pointer-extract pointer)
    (typecase obj
      (symbol
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer pointer :pointee obj :offset idx))
       (set obj new-object))
      (vector
       (setf (elt obj idx) new-object))
      ;; TODO: 
      ;; makes a lambda for a pointer to a reduced dimension array.
      (array				; FIXME
       (error 'pseudo-pointer-write-error
	      :pointer pointer :pointee obj :offset idx))
      (pseudo-pointer-to-place
       (unless (zerop idx)
	 (error 'pseudo-pointer-dangling-error
		:pointer pointer :pointee obj :offset idx))
       (funcall (pseudo-pointer-to-place-lambda obj) :set new-object))
      (otherwise
       (error 'pseudo-pointer-type-error :pointee obj)))))

(defun pseudo-pointer-invalidate (pointer)
  "Makes the POINTER to point no objects.  After that, calling
`pseudo-pointer-dereference' to this pointer will be error."
  (multiple-value-bind (obj idx base)
      (pseudo-pointer-extract pointer nil)
    (declare (ignore obj idx))
    (remhash base *pseudo-pointee-table*)))

;;; TODO:
;;; 1. Use weak hash-table. Its value may held by a weak pointer.
;;; 2. Defragments.
