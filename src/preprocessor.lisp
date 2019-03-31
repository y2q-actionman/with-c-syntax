(in-package #:with-c-syntax.core)

(let ((terminal-symbol-table (make-hash-table :test #'equal))
      (upcased-terminal-symbol-table (make-hash-table :test #'equal)))
  (loop for sym being the external-symbol of (find-syntax-package)
     as name = (symbol-name sym)
     as ucase = (string-upcase name)
     do (setf (gethash name terminal-symbol-table) sym)
     when (string/= name ucase)
     do (setf (gethash ucase upcased-terminal-symbol-table) sym))
  (defun intern-c-terminal (name case-spec)
    "Finds a symbol in `with-c-syntax.syntax' package having a same
name as NAME based on CASE-SPEC. If not found, returns `nil'."
    (or (gethash name terminal-symbol-table)
	(if (eq case-spec :upcase)
	    (gethash name upcased-terminal-symbol-table)))))

(defun intern-libc-symbol (name case-spec)
  "Finds a symbol in `with-c-syntax.libc' package having a same name
as NAME based on CASE-SPEC. If not found, returns `nil'."
  (let ((libc-package (find-package '#:with-c-syntax.libc)))
    (multiple-value-bind (sym sym-type) (find-symbol name libc-package)
      (when (and sym (eq sym-type :external))
	(return-from intern-libc-symbol sym)))
    (when (eq case-spec :upcase)
      (multiple-value-bind (sym sym-type)
	  (find-symbol (string-downcase name) libc-package)
	(when (and sym (eq sym-type :external))
	  (return-from intern-libc-symbol sym)))))
  nil)


(defconstant +preprocessor-macro+
  '+preprocessor-macro+
  "A symbol used as an indicator of `symbol-plist' holding the preprocessor function.
See `add-preprocessor-macro'.")

(defun find-preprocessor-macro (symbol)
  "Finds and returns a preprocessor macro definition named SYMBOL,
added by `add-preprocessor-macro'."
  (get symbol +preprocessor-macro+))

(defun add-preprocessor-macro (symbol value)
  "Establishes a new preprocessor macro to SYMBOL, which is corresponded to VALUE.

Preprocessor macros are held in `symbol-plist' of SYMBOL, and
indicated by `+preprocessor-macro+'."
  (setf (get symbol +preprocessor-macro+) value))

(defun remove-preprocessor-macro (symbol)
  "Removes a preprocessor macro named SYMBOL."
  (remprop symbol +preprocessor-macro+))

(defmacro define-preprocessor-symbol (name value &environment env)
  "Defines a new preprocessor symbol macro, named by NAME and its value is VALUE."
  (if (constantp value env)
      `(progn (defconstant ,name ,value)
	      (add-preprocessor-macro ',name ,value))
      (let ((eval (gensym)))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,eval ,value))
	     (defconstant ,name ,eval)
	     (eval-when (:load-toplevel :execute)
	       (add-preprocessor-macro ',name ,eval)))))))

(defmacro define-preprocessor-function (name lambda-list &body body)
  "Defined a new preprocessor function, named by NAME."
  (let ((pp-macro-function-name
	 (format-symbol (symbol-package name) "~A-PREPROCESSOR-MACRO" name))
	(values-sym (gensym "values")))
    `(progn
       (defun ,pp-macro-function-name (&rest ,values-sym)
	 ;; TODO: catch `destructuring-bind' error.
	 (destructuring-bind ,lambda-list ,values-sym
	   ,@body))
       (add-preprocessor-macro ',name #',pp-macro-function-name))))

(defun preprocessor-call-macro (lis-head fn)
  "A part of the `preprocessor' function."
  (let ((begin (pop lis-head)))
    (unless (string= begin "(")
      (error 'preprocess-error
	     :format-control "A symbol (~S) found between a preprocessor macro and the first '('"
	     :format-arguments (list begin))))
  (labels ((pop-next-or-error ()
             (unless lis-head
               (error 'preprocess-error
		      :format-control "Reached end of forms at finding preprocessor macro arguments."))
             (pop lis-head))
           (get-arg (start)
             (loop for i = start then (pop-next-or-error)
                until (or (string= i ",")
                          (if (string= i ")")
                              (progn (push i lis-head) t)))
                collect i)))
    (loop for i = (pop-next-or-error)
       until (string= i ")")
       collect (get-arg i) into args
       finally
         (return (values (apply fn args) lis-head)))))

(defun preprocessor (lis &optional (case-spec nil))
  "* Syntax
~preprocessor~ list-of-tokens &key allow-upcase-keyword => preprocesed-list

* Arguments and Values
- list-of-tokens       :: a list
- case-spec            :: ~:upcase~, or nil.
- preprocesed-list     :: a list

* Description
This function works like the C Preprocessor.

** Works.
At this stage, all of the functionalities of the standard CPP are not
implemented. Current working is below:

- Interning a symbol into this package when it has a same name as C
  keywords or operators, or libc symbols.

- Concatenation of string literals.

- Calling preprocessor macro (not recursive)
  Invoking preprocessor macros, defined by add-preprocessor-macro.

  This system is used for converting symbols denoting keywords of C
  belongs other packages to our package's one.
  
- A dirty hack for 'typedef'.
  Automatically adds 'void ;' after each typedef declarations.  This
  is a workaround for avoiding the problem between 'the lexer hack'
  and the look-aheading of cl-yacc.

~case-spec~ specifies how to intern C keywords or libc symbols.  If
~:upcase~ is specified, it tries to intern them in case-insensitive
way.


** Expansion
- If ~val~ is nil, no expansion is done. The original symbol is left.

- If ~val~ is a object not a function, it is used as an expansion.

- If ~val~ is a function, the function is called, and the returned
value is used for expansion. Arguments for the function are datum
between the following \( symbol and the next \) symbol, delimited
by \, symbol.

Example: If MAC is defined for a macro and its value is a function,
this sequence
  MAC \( a \, b b \, c c c \)
calls the function like:
  (funcall #'a-func-of-MAC '(a) '(b b) '(c c c))

* See Also
~find-preprocessor-macro~.

* Notes
- TODO: recursive expansion

- The feature 'If ~val~ is nil, no expansion is done.' is for
  implementing the recursive expansion in the future.  In CPP, an
  expanded macro is ignored in the next (recursive) expansion.  So, if
  a macro was expanded, I will push (the-expanded-macro . nil), and
  call the preprocessor recursively.
"
  (do (ret
       typedef-hack
       (token (pop lis) (pop lis)))
      ((and (null lis) ; (2019-2-24) I must check 'lis' here to get all symbols including NIL.
	    (null token))
       (nreverse ret))
    (declare (type list ret)
	     (type boolean typedef-hack))
    (when (symbolp token)
      ;; interning C keywords.
      (when-let ((c-op (intern-c-terminal (symbol-name token) case-spec)))
	(push c-op ret)
	(go processed!))		; I assume all no proprocessor macros defined.
      ;; interning libc keywords.
      (when-let ((lib-op (intern-libc-symbol (symbol-name token) case-spec)))
	(setf token lib-op))
      ;; preprocessor macro
      (when-let ((expansion (find-preprocessor-macro token)))
	(cond ((functionp expansion)	; preprocessor funcion
               (multiple-value-bind (ex-val new-lis)
		   (preprocessor-call-macro lis expansion)
		 (push ex-val ret)
		 (setf lis new-lis)
		 (go processed!)))
	      (t			; symbol expansion
               (push expansion ret)
	       (go processed!)))))
    ;; string concatenation
    (when (stringp token)
      (loop for next = (first lis)
	 while (stringp next)
	 collect (pop lis) into strs
	 finally
	   (push (apply #'concatenate 'string token strs) ret)
	   (go processed!)))
    ;; otherwise..
    (push token ret)

   processed!

    ;; typedef hack -- adds "void ;" after each typedef.
    (cond ((eq (first ret) '|typedef|)
	   (setf typedef-hack t))
	  ((and typedef-hack
		(eq (first ret) '\;))
	   (setf typedef-hack nil)
	   (revappendf ret '(|void| \;))))))
