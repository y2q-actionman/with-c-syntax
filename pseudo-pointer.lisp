(in-package :with-c-syntax)

(deftype pseudo-pointer ()
  'fixnum)

(defconstant +pseudo-pointer-mask+    #b1111111)
(defconstant +pseudo-pointer-safebit+ #b1000000)

;; integer -> (list :pointee obj :reader func :writer func)
(defvar *pseudo-pointee-table* (make-hash-table))
(defvar *pseudo-pointer-next* 0)

(defmacro with-pseudo-pointer-scope (() &body body)
  `(let ((*pseudo-pointee-table* (make-hash-table))
         (*pseudo-pointer-next* 0))
     ,@body))

(defun alloc-pseudo-pointer ()
  (incf *pseudo-pointer-next*) ; This makes the base of the first pointer to 0
  (let* ((base (ash *pseudo-pointer-next*
                    (logcount +pseudo-pointer-mask+)))
         (p (+ base +pseudo-pointer-safebit+)))
    (values base p)))

(defun pseudo-pointer-extract (p &optional (errorp t))
  (let* ((base (logandc2 p +pseudo-pointer-mask+))
	 (idx (- (logand p +pseudo-pointer-mask+)
		 +pseudo-pointer-safebit+))
	 (entry (gethash base *pseudo-pointee-table*)))
    (unless entry
      (when errorp (error "dangling pointer ~A" p)))
    (values entry idx base)))

(defun pseudo-pointer-pointee (p)
  (multiple-value-bind (entry)
      (pseudo-pointer-extract p)
    (getf entry :pointee)))

(defun pseudo-pointer-dereference (p)
  (multiple-value-bind (entry idx)
      (pseudo-pointer-extract p)
    (funcall (getf entry :reader)
	     (getf entry :pointee)
	     idx)))

(defun (setf pseudo-pointer-dereference) (val p)
  (multiple-value-bind (entry idx)
      (pseudo-pointer-extract p)
    (funcall (getf entry :writer)
	     (getf entry :pointee)
	     idx val)))

(defun make-pseudo-pointer* (pointee reader-fn writer-fn)
  (multiple-value-bind (base p)
      (alloc-pseudo-pointer)
    (setf (gethash base *pseudo-pointee-table*)
	  (list :pointee pointee
		:reader reader-fn
		:writer writer-fn))
    p))

;; Dynamically finds a handler.
(defun pphandle-symbol-reader (sym idx)
  (unless (zerop idx)
    (error "out of index to symbol-reference (~A)" idx))
  (symbol-value sym))

(defun pphandle-symbol-writer (sym idx val)
  (unless (zerop idx)
    (error "out of index to symbol-reference (~A)" idx))
  (set sym val))

(defun pphandle-sequence-reader (seq idx)
  (elt seq idx))

(defun pphandle-sequence-writer (seq idx val)
  (setf (elt seq idx) val))

(defun find-pseudo-pointer-handler (obj)
  (typecase obj
    (symbol
     (values #'pphandle-symbol-reader
	     #'pphandle-symbol-writer))
    (sequence
     (values #'pphandle-sequence-reader
	     #'pphandle-sequence-writer))
    (otherwise
     nil)))

(defun pseudo-pointer-pointable-p (obj)
  (find-pseudo-pointer-handler obj))

(defun make-pseudo-pointer (obj)
  (multiple-value-bind (reader writer)
      (find-pseudo-pointer-handler obj)
    (unless reader
      (error "pseudo pointers cannot hold this object ~S~%" obj))
    (make-pseudo-pointer* obj reader writer)))

