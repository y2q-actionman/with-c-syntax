(in-package :with-c-syntax)

(deftype pseudo-pointer ()
  'fixnum)

(defconstant +pseudo-pointer-mask+    #b1111111)
(defconstant +pseudo-pointer-safebit+ #b1000000)

;; integer -> (list :pointee obj :reader func :writer func)
(defvar *pseudo-pointee-table* (make-hash-table))
(defvar *pseudo-pointer-next* 0)

(defvar *pseudo-pointer-handlers*
  `((symbol :reader
            ,#'(lambda (sym idx)
                 (unless (zerop idx)
                   (error "out of index to symbol-reference (~A)" idx))
                 (symbol-value sym))
            :writer
            ,#'(lambda (sym idx val)
                 (unless (zerop idx)
                   (error "out of index to symbol-reference (~A)" idx))
                 (set sym val)))
    (sequence :reader
              ,#'(lambda (seq idx)
                   (elt seq idx))
              :writer
              ,#'(lambda (seq idx val)
                   (setf (elt seq idx) val)))))

(defun find-pseudo-pointer-handler (obj)
  (assoc obj *pseudo-pointer-handlers*
         :test #'typep))

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

(defun make-pseudo-pointer (pointee)
  (multiple-value-bind (base p)
      (alloc-pseudo-pointer)
    (let ((handler (find-pseudo-pointer-handler pointee)))
      (unless handler
	(error "pseudo pointers cannot hold this object ~S~%"
	       pointee))
      (setf (gethash base *pseudo-pointee-table*)
	    (list :pointee pointee
		  :reader (getf (cdr handler) :reader)
		  :writer (getf (cdr handler) :writer)))
      p)))

(defmacro with-pseudo-pointer-scope (() &body body)
  `(let ((*pseudo-pointee-table* (make-hash-table))
         (*pseudo-pointer-next* 0))
     ,@body))
