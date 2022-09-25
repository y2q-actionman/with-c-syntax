(in-package #:with-c-syntax.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun _-symbol-p (obj)
    "Returns T if OBJ is a symbol and starts with '_' character."
    (and (symbolp obj)
         (starts-with #\_ (symbol-name obj))))

  (defun collect-_-symbol-in-lambda-list (lambda-list)
    "Collects symbols starting with '_' character in LAMBDA-LIST."
    (multiple-value-bind (required optionals rest keywords allow-other-keys? auxs)
        (parse-ordinary-lambda-list lambda-list)
      (declare (ignore allow-other-keys?))
      (let ((_-in-required (remove-if-not #'_-symbol-p required))
            (_-in-optionals (remove-if-not #'_-symbol-p
                                           (mapcar #'first optionals)))
            (_-in-rest (if (_-symbol-p rest) (list rest) nil))
            (_-in-keywords (remove-if-not #'_-symbol-p
                                          (mapcar #'cadar ; (second (first x))
                                                  keywords)))
            (_-in-auxs (remove-if-not #'_-symbol-p
                                      (mapcar #'first auxs))))
        (nconc _-in-required _-in-optionals _-in-rest _-in-keywords _-in-auxs)))))

(defmacro lambda-ignoring-_ (lambda-list &body body)
  "Works like `cl:lambda' macro except automatically `declare's
 `ignore' for parameters beginning with '_' character."
  (let ((ignored-parameters
          (collect-_-symbol-in-lambda-list lambda-list)))
    (unless ignored-parameters
      (warn "No ignored parameters in: ~A" lambda-list))
    (multiple-value-bind (body decls doc)
        (parse-body body :documentation t)
      `(lambda ,lambda-list
         ,@ (if doc `(,doc))
         (declare (ignore ,@ignored-parameters))
         ,@decls
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-to-tail (lis i)
    (append lis (list i))))

(defun not-eql (x y)
  "Used for '!=' of C language."
  (not (eql x y)))

(defun reverse-ash (i c)
  (ash i (- c)))

(defun revappend-to (tail list)
  "Calls revappend with reversed order args. A helper for `revappendf'."
  (revappend list tail))

(defun nreconc-to (tail list)
  "Calls nreconc with reversed order args. A helper for `nreconcf'."
  (nreconc list tail))

(defmacro mv-cond-let ((&optional (var1 (gensym)) &rest rest-vars)
                       &body clauses)
  "This is like the famous 'COND-LET', but takes multiple values."
  (if (endp clauses)
      nil
      (let* ((clause1 (first clauses))
             (clause1-cond (first clause1))
             (clause1-body (or (rest clause1)
                               `((values ,var1 ,@rest-vars)))))
        `(multiple-value-bind (,var1 ,@rest-vars) ,clause1-cond
	   (declare (ignorable ,@rest-vars))
	   (if ,var1
	       (progn ,@clause1-body)
	       (mv-cond-let (,var1 ,@rest-vars) ,@(rest clauses)))))))

;;; Characters
;;; these definitions are used by '#{ }#' reader and libc (<ctype.h>).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +nul-character+
    (or (name-char "nul") (code-char 0)))

  (defconstant +vertical-tab-character+
    (or (name-char "vt") (code-char #x0b)))

  (defconstant +bel-character+
    (or (name-char "bel") (code-char #x07))))

(defun c-whitespace-p (char)
  (case char
    ((#\space #\page #\newline #\return #\tab ; These are whitespace in Lisp and C both.
	      #\linefeed  ; Only for Lisp. (Maybe same with #\newline)
	      #. +vertical-tab-character+) ; Only for C.
     t)
    (otherwise nil)))

(defun terminating-char-p (char &optional (readtable *readtable*))
  "Returns T if CHAR is a terminating character in READTABLE."
  ;; If a whitespace became a non-terminating macro char, this function returns nil.
  ;; (I think this behavior is correct for its name.)
  ;; Since their constituent trait is invalid, it will raise `reader-error' when read.
  (multiple-value-bind (fn non-terminating-p)
      (get-macro-character char readtable)
    (or (and fn (not non-terminating-p))
        (case char
          ((#\tab #\newline #\linefeed #\page #\return #\space)
           t)
          (otherwise
           nil)))))

;;; Modify macros

(define-modify-macro push-right (i)
  add-to-tail)

(define-modify-macro revappendf (list)
  revappend-to)

(define-modify-macro nreconcf (list)
  nreconc-to)

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
  "A post increment version of `incf'."
  ;; TODO: use `serapeum:define-post-modify-macro' of in serapeum.
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion form env)
    (with-gensyms (ret delta-tmp)
      `(let* (,@(mapcar #'list dummies vals)
	      (,ret ,getter)
	      (,delta-tmp ,delta)
	      (,(car newval) (+ ,ret ,delta-tmp)))
	 (prog1 ,ret
	   ,setter)))))
 
;;; Array dimensions

(defun make-dimension-list (dims &optional default)
  "Constructs a nested list like `make-array'."
  (if dims
      (loop repeat (car dims)
         collect (make-dimension-list (cdr dims) default))
      default))

(defun ref-dimension-list (lis dim-1 &rest dims)
  "Accesses a nested list like `aref', as a multi-dimensional array."
  (if (null dims)
      (nth dim-1 lis)
      (apply #'ref-dimension-list (nth dim-1 lis) (car dims) (cdr dims))))

(defun (setf ref-dimension-list) (val lis dim-1 &rest dims)
  "Accesses a nested list like `aref', as a multi-dimensional array."
  (if (null dims)
      (setf (nth dim-1 lis) val)
      (setf (apply #'ref-dimension-list (nth dim-1 lis) (car dims) (cdr dims))
            val)))
  
(defun dimension-list-max-dimensions (lis)
  "Calculates max lengths per depth of a nested list, like `array-dimensions'"
  (let ((max-depth 0)
        (dim-table (make-hash-table :test 'eq))) ; (depth . max-len)
    (labels ((dim-calc (depth lis)
               (maxf max-depth depth)
               (maxf (gethash depth dim-table 0) (length lis))
               (loop for i in lis
                  when (consp i)
                  do (dim-calc (1+ depth) i))))
      (dim-calc 0 lis))
    (loop for i from 0 to max-depth
       collect (gethash i dim-table))))
    
(defun make-dimension-list-load-form (lis max-depth)
  "Makes a load form of a nested list."
  (if (or (atom lis)
          (<= max-depth 0))
      lis
      `(list
        ,@(loop for i in lis
             collect (make-dimension-list-load-form i (1- max-depth))))))
  
(defun make-reduced-dimension-array (array &rest subscripts)
  "Makes a displaced array which has a reduced dimensions.

Example: Consider what is returned by
  (make-reduced-dimension-array (make-array '(2 2 2)) '(1))

Its dimension is '(2 2), and it is a displaced array aliasing from
'(1 0 0) to '(1 2 2) in the original array.
"
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
