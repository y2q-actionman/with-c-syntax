(in-package #:with-c-syntax.core)

;;; C punctuators.

(defun find-punctuator (name)
  (find-symbol name '#:with-c-syntax.punctuator))

(defun find-digraph-replacement (punctuator)
  (case punctuator
    (with-c-syntax.punctuator:|<:| 'with-c-syntax.syntax:[)
    (with-c-syntax.punctuator:|:>| 'with-c-syntax.syntax:])
    (with-c-syntax.punctuator:|<%| 'with-c-syntax.syntax:{)
    (with-c-syntax.punctuator:|%>| 'with-c-syntax.syntax:})
    (with-c-syntax.punctuator:|%:| 'with-c-syntax.punctuator:|#|)
    (with-c-syntax.punctuator:|%:%:| 'with-c-syntax.punctuator:|##|)))

(defvar *with-c-syntax-preprocessor-process-digraph* nil
  "Determines whether preprocessor replaces digraphs.
 If this is true, replacement occurs but `with-c-syntax-style-warning' is signalled.
 If this is `:no-warn', replacement occurs and the style-warning is not signalled.")

;;; C syntax words

(defun find-c-terminal (name)
  "Find a symbol in `with-c-syntax.syntax' package having a same
NAME. If not found, returns `nil'."
  (find-symbol name '#:with-c-syntax.syntax))

(defvar *cache-for-find-c-terminal-upcase*
  (loop with cache = (make-hash-table :test #'equal)
        for sym being the external-symbol of '#:with-c-syntax.syntax
        as upcase-name = (string-upcase (symbol-name sym))
        do (setf (gethash upcase-name cache) sym)
        finally (return cache)))

(defun find-c-terminal-by-upcased-name (name)
  "Find a symbol in `with-c-syntax.syntax' package having a same
 upcased NAME. If not found, returns `nil'."
  (gethash name *cache-for-find-c-terminal-upcase*))

;;; Preprocessor macro.

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

(defmacro define-preprocessor-constant (name value &environment env)
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

(defun preprocessor-equal-p (token name)
  (and (symbolp token)
       (string= token name)))

(defun collect-preprocessor-macro-arguments (lis-head)
  "A part of the `preprocessor' function."
  (let ((begin (pop lis-head)))
    (unless (preprocessor-equal-p begin "(")
      (error 'preprocess-error
	     :format-control "A symbol (~S) found between a preprocessor macro and the first '('"
	     :format-arguments (list begin))))
  (labels
      ((pop-or-error ()
         (unless lis-head
           (error 'preprocess-error
		  :format-control "Reached end of forms at finding preprocessor macro arguments."))
         (pop lis-head))
       (collect-one-arg (start)
         (loop with nest-level of-type fixnum = 0
	    for j = start then (pop-or-error)
	    do (cond ((preprocessor-equal-p j "(")
		      (incf nest-level))
		     ((preprocessor-equal-p j ",")
		      (loop-finish))
		     ((preprocessor-equal-p j ")")
		      (when (minusp (decf nest-level))
			(push j lis-head)
			(loop-finish))))
            collect j)))
    (loop for i = (pop-or-error)
       until (preprocessor-equal-p i ")")
       collect (collect-one-arg i) into macro-args
       finally
         (return (values macro-args lis-head)))))

(defmacro mv-cond-let ((&rest vars) &body clauses)
  "This is like the famous 'COND-LET', but takes multiple values."
  (let ((clause1 (first clauses)))
    (cond
      ((null clauses) nil)
      ((length= 1 clause1)
       `(multiple-value-bind (,@vars) ,(first clause1)
	  (declare (ignorable ,@(rest vars)))
	  (if ,(first vars)
	      (values ,@vars)
	      (mv-cond-let (,@vars) ,@(rest clauses)))))
      ((eql (first clause1) t)
       `(progn ,@(rest clause1)))
      (t
       `(multiple-value-bind (,@vars) ,(first clause1)
	  (declare (ignorable ,@(rest vars)))
	  (if ,(first vars)
	      (progn ,@(rest clause1))
	      (mv-cond-let (,@vars) ,@(rest clauses))))))))

(defun intern-to-its-package (name symbol)
  "`intern' NAME into the package of SYMBOL."
  (intern name (symbol-package symbol)))

(defun find-operator-on-prefix (operator symbol)
  "This function tries to find OPERATOR in front of SYMBOL.
If found, it returns T and list of of splited symbols.  If not found,
it returns NIL."
  (multiple-value-bind (found suffix)
      (starts-with-subseq (symbol-name operator) (symbol-name symbol)
			  :return-suffix t)
    (if found
	(values t
		(list operator
		      (intern-to-its-package suffix symbol))))))

(defun find-operator-on-suffix (operator symbol)
  "This function tries to find OPERATOR in back of SYMBOL.
If found, it returns T and list of of splited symbols.  If not found,
it returns NIL."
  (let ((sym-name (symbol-name symbol))
	(op-name (symbol-name operator)))
    (if (ends-with-subseq op-name sym-name)
	(values t
                (list (intern-to-its-package
		       (subseq sym-name 0 (- (length sym-name) (length op-name)))
		       symbol)
		      operator)))))

(defun earmuff-lengthes (string earmuff-char)
  "Calculates '*earmuff*' lengthes in STRING with EARMUFF-CHAR, and
returns it lengthes in front and in last.

  e.g. (earmuff-lengthes \"*foo**\" #\*) ; => (values 1 2)"
  (let* ((len (length string))
         (prefix-len
          (if (char/= (char string 0) earmuff-char)
              0                       ; not found
              (or (position earmuff-char string :test #'char/=)
                  len))) ; STRING is fully filled with the EARMUFF-CHAR.
         (suffix-len
          (if (char/= (char string (1- len)) earmuff-char)
              0				; not found
              (if-let (suffix-pos--1 (position earmuff-char string :test #'char/= :from-end t))
		(- len (1+ suffix-pos--1))
                len))))   ; STRING is fully filled with the EARMUFF-CHAR.
    (values prefix-len suffix-len)))

(defun preprocessor-try-split (symbol)	; TODO: consider this name
  "This function tries to find some C operators in SYMBOL.
If found, it returns T and list of splited symbols. If not found, it
returns NIL."
  ;; I think operators their precedence is equal or more than '~'
  ;; are candidates, in my personal C style.
  (mv-cond-let (found trimed)
    ;; -- Operator precedence 1 --
    ;; TODO: add '.' and '->' operator.
    ((find-operator-on-suffix 'with-c-syntax.syntax:++ symbol))
    ((find-operator-on-suffix 'with-c-syntax.syntax:-- symbol))
    ;; -- Operator precedence 2 --
    ((find-operator-on-prefix 'with-c-syntax.syntax:++ symbol))
    ((find-operator-on-prefix 'with-c-syntax.syntax:-- symbol))
    ((find-operator-on-prefix 'with-c-syntax.syntax:|sizeof| symbol))
    ;; I think these are rarely used in Lisp symbols.
    ((find-operator-on-prefix 'with-c-syntax.syntax:- symbol))
    ((find-operator-on-prefix 'with-c-syntax.syntax:! symbol))
    ((find-operator-on-prefix 'with-c-syntax.syntax:~ symbol))
    ((find-operator-on-prefix 'with-c-syntax.syntax:& symbol))
    ;; These are used as *earmuffs*. I handle them specially.
    ((find-operator-on-prefix 'with-c-syntax.syntax:* symbol)
     (multiple-value-bind (prefix-len suffix-len)
         (earmuff-lengthes (symbol-name symbol) #\*)
       (if (> prefix-len suffix-len)
           (values t trimed)
           (values nil symbol))))
    ((find-operator-on-prefix 'with-c-syntax.syntax:+ symbol)
     (multiple-value-bind (prefix-len suffix-len)
         (earmuff-lengthes (symbol-name symbol) #\+)
       (if (> prefix-len suffix-len)
           (values t trimed)
           (values nil symbol))))
    ;; FIXME: What to do when (>= prefix-len 2) or (>= suffix-len 2) ?
    ;; Should I accept 'int n = ++a-constant++ 10' as '(+ (+ +a-constant+) 10)'?
    (t
     (values nil symbol))))

;;; preprocessor-state object held in the main loop.

(defstruct (preprocessor-state (:conc-name pp-state-)) 
  (token-list nil :type list)
  (readtable-case :upcase :type keyword :read-only t)
  (result-list nil :type list)
  (file-pathname nil :type t)
  (line-number 0 :type integer)
  (tokens-in-line 0 :type integer)
  (typedef-hack? nil :type boolean)
  (process-digraph? nil :type boolean :read-only t))

(defmacro with-preprocessor-state-slots ((state) &body body)
  (once-only (state)
    `(symbol-macrolet ((token-list (pp-state-token-list ,state))
                       (readtable-case (pp-state-readtable-case ,state))
                       (result-list (pp-state-result-list ,state))
                       (line-number (pp-state-line-number ,state))
                       (tokens-in-line (pp-state-tokens-in-line ,state))
                       (typedef-hack? (pp-state-typedef-hack? ,state))
                       (process-digraph? (pp-state-process-digraph? ,state)))
       ,@body)))

(defun preprocessor-loop-try-intern-punctuators (state token)
  "Intern puctuators. this is with-c-syntax specific preprocessing path."
  (with-preprocessor-state-slots (state)
    (when-let (punctuator (find-punctuator (symbol-name token)))
      (when-let ((replace (find-digraph-replacement punctuator))) ; Check digraph 
        (unless (eq process-digraph? :no-warn)
          (warn 'with-c-syntax-style-warning
                :message (format nil "Digraph sequence '~A' (means ~A) found."
                                 punctuator replace)))
        (when process-digraph?
          (setf punctuator replace)))
      (push punctuator result-list)
      ;; typedef hack -- adds "void ;" after each typedef.
      (when (and typedef-hack?
	         (eq punctuator '\;))
        (setf typedef-hack? nil)
        (revappendf result-list '(|void| \;)))
      t)))

(defun preprocessor-loop-try-intern-keywords (state token)
  "Intern C keywords. this is with-c-syntax specific preprocessing path."
  (with-preprocessor-state-slots (state)
    (when-let (c-op (ecase readtable-case ; FIXME: cleanup here.
                      ((:upcase :invert)
                       (find-c-terminal-by-upcased-name (symbol-name token)))
                      ((:downcase :preserve)
                       (find-c-terminal (symbol-name token)))))
      (push c-op result-list)
      (when (eq c-op '|typedef|)
        (setf typedef-hack? t)) ; enables 'typedef hack'.
      t)))
  
(defun preprocessor-loop-concatenate-string (state token)
  "Do the translation phase 6 -- string concatenation."
  (with-preprocessor-state-slots (state)
    (let* ((following-strings
             (loop 
               for next = (first token-list)
	       while (stringp next)
	       collect (pop token-list)))
           (string-list
             (list* token following-strings)))
      ;; The previous string, may be generated by macros, is also a target. 
      (when (stringp (first result-list))
        (push (pop result-list)
              string-list))
      (push (apply #'concatenate 'string string-list)
            result-list))))

(defun preprocessor-loop (state)
  "See `preprocessor'"
  (with-preprocessor-state-slots (state)
    (do ((token (pop token-list) (pop token-list)))
        ((and (null token-list) ; I must check 'token-list' here to get all symbols, including NIL.
	      (null token))
         state)
      (typecase token
        (symbol
         (cond
           ((eq token +newline-marker+)
            (progn))
           ;; Part of translation Phase 4 -- preprocessor macro
           ((when-let (pp-macro (find-preprocessor-macro token))
              (cond ((functionp pp-macro) ; preprocessor funcion
	             (multiple-value-bind (macro-arg new-lis)
		         (collect-preprocessor-macro-arguments token-list)
		       (push (apply pp-macro macro-arg) result-list)
		       (setf token-list new-lis)))
	            (t			; symbol expansion
                     (push pp-macro result-list)))))
           ;; with-c-syntax specifics.
           ((preprocessor-loop-try-intern-punctuators state token)
            t)
           ((preprocessor-loop-try-intern-keywords state token)
            t)
           ;; with-c-syntax specific: Try to split the token.
           ;; FIXME: I think this should be only for reader level 1.
           ((unless (or (boundp token)
                        (fboundp token))
              (multiple-value-bind (splited-p results) (preprocessor-try-split token)
	        (if splited-p
	            (setf token-list (nconc results token-list))
                    nil))))             ; fallthrough
           (t
            (push token result-list))))
        (string
         (preprocessor-loop-concatenate-string state token))
        (t
         (push token result-list)))
      (cond ((eq token +newline-marker+)
             (incf line-number)
             (setf tokens-in-line 0))
            (t
             (incf tokens-in-line))))))

(defun preprocessor (token-list readtable-case input-file-pathname
                     &key (process-digraph *with-c-syntax-preprocessor-process-digraph*))
  "This function preprocesses TOKEN-LIST before parsing.

Current workings are below:

- Interning a symbol into this package when it has a same name as C
  keywords or operators, or libc symbols.
  READTABLE-CASE affects how to do it.

- Concatenation of string literals.

- Calling preprocessor macros, defined by `add-preprocessor-macro'.
  How preprocessor macros are expanded is described below.

- A dirty hack for 'typedef'.
  Automatically adds 'void ;' after each typedef declarations.  This
  is a workaround for avoiding the problem between 'the lexer hack'
  and the look-aheading of cl-yacc.


Expansion rules:

- If VALUE is nil, no expansion is done. The original symbol is left.

- If VALUE is a object not a function, it is used as an expansion.

- If VALUE is a function, the function is called, and the returned
value is used for expansion. Arguments for the function are datum
between the following \( symbol and the next \) symbol, delimited
by \, symbol.

Example: If MAC is defined for a macro and its value is a function,
this sequence
  MAC \( a \, b b \, c c c \)
calls the function like:
  (funcall #'a-func-of-MAC '(a) '(b b) '(c c c))"
  (let* ((pp-state
           (make-preprocessor-state
            :token-list token-list
            :readtable-case readtable-case
            :file-pathname input-file-pathname 
            :process-digraph? process-digraph))
         (pp-state
           (preprocessor-loop pp-state)))
    (nreverse (pp-state-result-list pp-state))))

;;; NOTE:
;;; I don't plan to implemente the C preprocessor fully.
;;; To explain the reason, Let's see the CPP works
;;; (from https://en.wikipedia.org/wiki/C_preprocessor).
;;; 
;;; 1. Handling trigraphs.
;;;    This is not needed because all '# \ ^ [ ] | { } ~' characters
;;;    are in the standard characters of CL.
;;; 2. Line splicing.
;;;    This is done by the Lisp reader.
;;; 3. Tokenization.
;;;    This is done by the Lisp reader also.
;;; 4. Macro Expansion and Directive Handling.
;;;    I think all directives can be replaced:
;;;    '#include'   -- `LOAD' or '#.' syntax.
;;;    '#if' family -- '#+', '#-', or `defmacro's.
;;;    '#define'    -- `defmacro'.
;;;    '#undef'     -- `fmakunbound'.
;;;    '#error'     --  `error'.
;;;    '#' directive -- `#.(string ...)'
;;;    '##' directive -- calling `intern' in macros.
;;;
;;; And its macro-expansing rule is quite difficult. I felt it is unworthy to make it.
;;; 
;;; However, I leave here old comments:
;;; ----
;;; The feature 'If ~val~ is nil, no expansion is done.' is for
;;; implementing the recursive expansion in the future.  In CPP, an
;;; expanded macro is ignored in the next (recursive) expansion.  So,
;;; if a macro was expanded, I will push (the-expanded-macro . nil),
;;; and call the preprocessor recursively.
