(in-package :with-c-syntax)

(deftype pseudo-pointer ()
  'fixnum)

(defconstant +pseudo-pointer-mask+    #b0111111)
(defconstant +pseudo-pointer-safebit+ #b0100000)

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
  (let* ((base (ash *pseudo-pointer-next*
                    (logcount +pseudo-pointer-mask+)))
         (p (+ base +pseudo-pointer-safebit+)))
    (incf *pseudo-pointer-next*)
    (values base p)))

(defun pseudo-pointer-extract (p)
  (let ((base (logandc2 p +pseudo-pointer-mask+))
        (idx (- (logand p +pseudo-pointer-mask+)
                +pseudo-pointer-safebit+)))
    (values base idx)))

(defun pseudo-pointer-pointee (p)
  (multiple-value-bind (base idx)
      (pseudo-pointer-extract p)
    (declare (ignore idx))
    (let ((entry (gethash base *pseudo-pointee-table*)))
      (unless entry
        (error "dangling pointer ~A" p))
      (getf entry :pointee))))

(defun pseudo-pointer-dereference (p)
  (multiple-value-bind (base idx)
      (pseudo-pointer-extract p)
    (let ((entry (gethash base *pseudo-pointee-table*)))
      (unless entry
        (error "dangling pointer ~A" p))
      (funcall (getf entry :reader)
	       (getf entry :pointee)
               idx))))

(defun (setf pseudo-pointer-dereference) (val p)
  (multiple-value-bind (base idx)
      (pseudo-pointer-extract p)
    (let ((entry (gethash base *pseudo-pointee-table*)))
      (unless entry
        (error "dangling pointer ~A" p))
      (funcall (getf entry :writer)
	       (getf entry :pointee)
	       idx val))))

(defun make-pseudo-pointer* (&rest pointee-candidates)
  (multiple-value-bind (base p)
      (alloc-pseudo-pointer)
    (loop for pointee in pointee-candidates
       as handler = (find-pseudo-pointer-handler pointee)
       when handler
       do (setf (gethash base *pseudo-pointee-table*)
                (list :pointee pointee
                      :reader (getf (cdr handler) :reader)
                      :writer (getf (cdr handler) :writer)))
         (return p)
       finally
         (error "pseudo pointers cannot hold this object ~S~%"
                pointee-candidates))))

(defun make-pseudo-pointer (pointee)
  (make-pseudo-pointer* pointee))

(defmacro with-pseudo-pointer-scope (() &body body)
  `(let ((*pseudo-pointee-table* (make-hash-table))
         (*pseudo-pointer-next* 0))
     ,@body))
