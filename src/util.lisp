(in-package #:with-c-syntax.core)

;; These are referenced by the parser directly.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append-item-to-right (lis i)
    (append lis (list i)))

  (defun concatinate-comma-list (lis op i)
    (declare (ignore op))
    (append-item-to-right lis i))
)

(defun reverse-ash (i c)
  (ash i (- c)))

;; modify macros
(define-modify-macro push-right (i)
  append-item-to-right)

(define-modify-macro mulf (&rest args)
  *)

(define-modify-macro divf (&rest args)
  /)

(define-modify-macro modf (&rest args)
  mod)

(define-modify-macro ashf (shift)
  ash)

(define-modify-macro reverse-ashf (shift)
  reverse-ash)

(define-modify-macro logandf (&rest args)
  logand)

(define-modify-macro logxorf (&rest args)
  logxor)

(define-modify-macro logiorf (&rest args)
  logior)

(defmacro post-incf (form &optional (delta 1) &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion form env)
    (with-gensyms (ret delta-tmp)
      `(let* (,@(mapcar #'list dummies vals)
	      (,ret ,getter)
	      (,delta-tmp ,delta)
	      (,(car newval) (+ ,ret ,delta-tmp)))
	 (prog1 ,ret
	   ,setter)))))

;; (name me!)
(defmacro with-dynamic-bound-symbols ((&rest symbols) &body body)
  ;; If no symbols, removes PROGV.
  ;; This makes faster codes.
  (if (null symbols)
      `(progn ,@body)
      `(progv ',symbols (list ,@symbols)
	 (locally (declare (special ,@symbols))
	   ,@body))))

;; treats a nested lists as an multi-dimentional array.
(defun make-dimension-list (dims &optional default)
  (if dims
      (loop repeat (car dims)
         collect (make-dimension-list (cdr dims) default))
      default))

(defun ref-dimension-list (lis dim-1 &rest dims)
  (if (null dims)
      (nth dim-1 lis)
      (apply #'ref-dimension-list (nth dim-1 lis) (car dims) (cdr dims))))

(defun (setf ref-dimension-list) (val lis dim-1 &rest dims)
  (if (null dims)
      (setf (nth dim-1 lis) val)
      (setf (apply #'ref-dimension-list (nth dim-1 lis) (car dims) (cdr dims))
            val)))
  
(defun dimension-list-max-dimensions (lis)
  (let ((max-depth 0)
        (dim-table (make-hash-table :test 'eq))) ; (depth . max-len)
    (labels ((dim-calc (depth lis)
               (maxf max-depth depth)
               (maxf (gethash depth dim-table -1) (length lis))
               (loop for i in lis
                  when (and i (listp i))
                  do (dim-calc (1+ depth) i))))
      (dim-calc 0 lis))
    (loop for i from 0 to max-depth
       collect (gethash i dim-table))))
    
(defun make-dimension-list-load-form (lis max-depth)
  (if (or (null lis)
          (atom lis)
          (zerop max-depth))
      lis
      `(list
        ,@(loop for i in lis
             collect (make-dimension-list-load-form i (1- max-depth))))))
  
;; array
(defun make-reduced-dimension-array (array &rest subscripts)
  (let* ((array-dims (array-dimensions array))
	 (new-array-dimensions
	  (nthcdr (length subscripts) array-dims))
	 (new-array-start-subscripts
	  (append subscripts
		  (make-list (length new-array-dimensions)
			     :initial-element 0)))
	 (new-array-start-rm-index
	  (apply #'array-row-major-index array new-array-start-subscripts)))
    (make-array new-array-dimensions
		:element-type (array-element-type array)
		:displaced-to array
		:displaced-index-offset new-array-start-rm-index)))

;; used in reader
(defun make-string-from-chars (&rest chars)
  (coerce chars 'string))

(defun terminating-char-p (char &optional (readtable *readtable*))
  (case char
    ((#\tab #\newline #\linefeed #\page #\return #\space)
     t)
    (otherwise
     (multiple-value-bind (fn non-terminating-p)
	 (get-macro-character char readtable)
       (and fn (not non-terminating-p))))))

(define-constant +bracket-pair-alist+
    '((#\( . #\)) (#\[ . #\])
      (#\{ . #\}) (#\< . #\>))
  :test 'equal)
