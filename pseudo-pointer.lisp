(in-package :cl-user)

(defvar *pseudo-pointee-table* (make-hash-table))
(defvar *pseudo-pointer-next* 0)
(defconstant +pseudo-pointer-mask+    #b0111111)
(defconstant +pseudo-pointer-safebit+ #b0100000)

(defun alloc-pseudo-pointer ()
  (let* ((p-base (ash (incf *pseudo-pointer-next*)
		      (logcount +pseudo-pointer-mask+)))
	 (p (logior p-base +pseudo-pointer-safebit+)))
    p))

(defun pseudo-pointer-dereference (p)
  (let ((entry
	 (gethash (logandc2 p +pseudo-pointer-mask+)
		  *pseudo-pointee-table*)))
    (when entry
      (funcall (getf entry :reader)
	       (getf entry :pointee)))))

(defun (setf pseudo-pointer-dereference) (val p)
  (let ((entry
	 (gethash (logandc2 p +pseudo-pointer-mask+)
		  *pseudo-pointee-table*)))
    (when entry
      (funcall (getf entry :writer)
	       val
	       (getf entry :pointee)))))

(defun make-pseudo-pointer (pointee)
  (let ((p (alloc-pseudo-pointer)))
    (setf (gethash (logandc2 p +pseudo-pointer-mask+)
		   *pseudo-pointee-table*)
	  (list :pointee pointee
		:reader nil		; typecase ...
		:writer nil))		; typecase ...
    p))

;; TODO: use progv to get a reference to automatic variable..