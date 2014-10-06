(in-package :with-c-syntax)

;; Standard Common Lisp types.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +standardized-atomic-type-specifiers+
      '(arithmetic-error array atom
	base-char base-string bignum bit bit-vector
	broadcast-stream built-in-class
	cell-error character class compiled-function complex
	concatenated-stream condition cons control-error
	division-by-zero double-float
	echo-stream end-of-file error extended-char
	file-error file-stream fixnum float
	floating-point-inexact floating-point-invalid-operation
	floating-point-overflow floating-point-underflow
	function
	generic-function
	hash-table
	integer
	keyword
	list logical-pathname long-float
	method method-combination
	nil null number
	package package-error parse-error pathname
	print-not-readable program-error
	random-state ratio rational reader-error readtable
	real restart
	sequence serious-condition short-float signed-byte
	simple-array simple-base-string simple-bit-vector
	simple-condition simple-error simple-string
	simple-type-error simple-vector simple-warning
	single-float standard-char standard-class
	standard-generic-function standard-method
	standard-object storage-condition
	stream stream-error string string-stream
	structure-class structure-object
	style-warning symbol synonym-stream
	t two-way-stream type-error
	unbound-slot unbound-variable undefined-function
	unsigned-byte
	vector
	warning)
    :test 'equal
    :documentation "Hyperspec Figure 4-2.")

  (define-constant +standardized-compound-type-specifier-names+
      '(and array
	base-string bit-vector
	complex cons
	double-float
	eql
	float function
	integer
	long-float
	member mod
	not
	or
	rational real
	satisfies short-float signed-byte simple-array
	simple-base-string simple-bit-vector simple-string
	simple-vector single-float string
	unsigned-byte
	values vector)
    :test 'equal
    :documentation "Hyperspec Figure 4-3.")
  
  (define-constant +standardized-compound-only-type-specifier-names+
      '(and eql member
	mod not or
	satisfies values)
    :test 'equal
    :documentation "Hyperspec Figure 4-4.")
  
  (define-constant +standardized-type-specifier-names+
      (sort (copy-list
	     (union +standardized-atomic-type-specifiers+
		    +standardized-compound-type-specifier-names+
		    :test #'eq))
	    #'string<)
      :test 'equal
      :documentation "Hyperspec Figure 4-6."))

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
(define-modify-macro append-item-to-right-f (i)
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

;; treats a nested lists as an multid-imentional array.
(defun make-dimension-list (dims &optional default)
  (if dims
      (loop for i from 0 below (car dims)
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

(defun standard-whitespace-p (char)
  (and (standard-char-p char)
       (case char
	 ((#\space #\newline) t)
	 (otherwise nil))))
