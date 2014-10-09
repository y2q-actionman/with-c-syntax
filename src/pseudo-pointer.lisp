(in-package #:with-c-syntax)

(deftype pseudo-pointer (&optional (pointee-type t))
  (declare (ignore pointee-type))
  'fixnum)

(defconstant +pseudo-pointer-mask+    #b1111111)
(defconstant +pseudo-pointer-safebit+ #b1000000)

(defvar *pseudo-pointee-table* (make-hash-table :test 'eq))
(defvar *pseudo-pointer-next* 0)

(defmacro with-pseudo-pointer-scope (() &body body)
  `(let ((*pseudo-pointee-table* (make-hash-table :test 'eq))
         (*pseudo-pointer-next* 0))
     ,@body))

(defun alloc-pseudo-pointer (pointee)
  (incf *pseudo-pointer-next*) ; This makes the base of the first pointer to 0
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
