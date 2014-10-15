(in-package #:with-c-syntax.core)

(defun preprocessor-initial-set ()
  (flet ((make-entry (sym)
           (cons (symbol-name sym) sym)))
    (nconc (mapcar #'make-entry +operators+)
           (mapcar #'make-entry +keywords+))))

(defun preprocessor-initial-set-for-upcase ()
  (loop for (name . macro) in (preprocessor-initial-set)
     as ucase = (string-upcase name)
     when (string/= name ucase)
     collect (cons ucase macro)))

(defvar *preprocessor-macro*
  (preprocessor-initial-set)
  "* Value Type
alist :: <symbol or string> -> <nil, function, symbol, or any literal object>.

* Description
This variable holds preprocessor macro definitions.
")

(defvar *preprocessor-macro-for-upcase*
  (preprocessor-initial-set-for-upcase)
  "* Value Type
alist :: <symbol or string> -> <nil, function, symbol, or any literal object>.

* Description
This variable holds preprocessor macro definitions, used when current
readtable-case is :upcase
")

(defun preprocessor-macro-compare (x y)
  (if (and (symbolp x) (symbolp y))
      (eq x y)
      (string= x y)))

(defun preprocessor-call-macro (lis-head fn)
  (let ((begin (pop lis-head)))
    (unless (string= begin '|(|)
      (error "some symbols (~S) found between preprocessor macro and the first '('"
             begin)))
  (labels ((pop-next-or-error ()
             (unless lis-head
               (error "reached end of symbols at finding preprocessor macro args"))
             (pop lis-head))
           (get-arg (start)
             (loop for i = start then (pop-next-or-error)
                until (or (string= i '|,|)
                          (if (string= i '|)|)
                              (progn (push i lis-head) t)))
                collect i)))
    (loop for i = (pop-next-or-error)
       until (string= i '|)|)
       collect (get-arg i) into args
       finally
         (return (values (apply fn args) lis-head)))))

(defun preprocessor (lis &key allow-upcase-keyword)
  "* Syntax
~preprocessor~ list-of-tokens &key allow-upcase-keyword => preprocesed-list

* Arguments and Values
- list-of-tokens       :: a list
- allow-upcase-keyword :: a generalized boolean
- preprocesed-list     :: a list

* Description
This function works like the C Preprocessor.  At this stage, not all
of its functionalities are implemented.  Current working is below:

- Concatenation of string literals.

- Calling preprocessor macro (not recursive)
  Invoking preprocessor macros, defined by define-preprocessor-macro.

  This system is used for converting symbols denoting keywords of C
  belongs other packages to our package's one.
  
- A dirty hack for 'typedef'.
  Autimatically addes 'void ;' after each typedef declarations.  This
  is a workaround for avoiding the problem between 'the lexer hack'
  and the look-aheading of cl-yacc.

If ~allow-upcase-keyword~ is t, enables preprocessor macros for upcase
symbols.

* Notes
- TODO: recursive expansion
"
  (loop with ret = nil
     with typedef-hack = nil
     for i = (pop lis)
     while i
     ;; preprocessor macro
     when (symbolp i)
     do (when-let*
            ((entry
              (or (assoc i *preprocessor-macro*
                         :test #'preprocessor-macro-compare)
                  (if allow-upcase-keyword
                      (assoc i *preprocessor-macro-for-upcase*
                             :test #'preprocessor-macro-compare))))
             (expansion (cdr entry)))
          (cond ((null expansion)     ; no-op
                 nil)                   
                ((functionp expansion) ; preprocessor funcion
                 (multiple-value-bind (ex-val new-lis)
                     (preprocessor-call-macro lis expansion)
                   (push ex-val ret)
                   (setf lis new-lis)
                   (setf i nil)))
                (t                  ; symbol expansion
                 (push expansion ret)
                 (setf i nil))))
     ;; string concatenation
     when (stringp i)
     do (loop with str = i
           for next = (first lis)
           while (stringp next)
           do (pop lis)
             (setf str (concatenate 'string str next))
           finally
             (push str ret)
             (setf i nil))
     ;; otherwise..
     when i
     do (push i ret)

     ;; typedef hack -- addes "void \;" after each typedef.
     if (eq (first ret) '|typedef|)
     do (setf typedef-hack t)
     else if (and typedef-hack
                  (eq (first ret) '\;))
     do (setf typedef-hack nil)
       (push '|void| ret)
       (push '\; ret)
     end

     finally
       (return (nreverse ret))))

(defun define-preprocessor-macro (name val &optional for-upcase)
  "* Syntax
~define-preprocessor-macro~ name val &optional for-upcase => macro-def

* Arguments and Values
- name       :: a string or symbol.
- val        :: an object.
- for-upcase :: a generalized boolean.
- macro-def  :: an object (same as ~val~)

* Description
This function establishes a preprocessor macro.  When the preprocessor
finds a symbol matching ~name~, it is expanded by ~val~.

If ~for-upcase~ is t, a macro is defined for upcase symbols.

** Matching rule
- If ~name~ is a string, the matching functions is 'string='. Packages
are ignored.

- If ~name~ is a symbol, the matching functions is 'eq'. This is
package-aware.

** Expansion
- If ~val~ is nil, no expansion is done. The original symbol is left.

 (This feature is for implementing the recursive expansion in the
 future.  In CPP, a expanded macro is ignored in the next (recursive)
 expansion.  So, if a macro was expanded, I will push
 (the-expanded-macro . nil), and call the preprocessor recursively.)

- If ~val~ is a object not a function, it is used as an expansion.

- If ~val~ is a function, the function is called, and the returned
value is used for expansion. Arguments for the function are datum
between the following '(' symbol and the next ')' symbols, delimited
by \, symbol.

Example: If MAC is defined for a macro and its value is a function,
this sequence
  MAC \( a \, b b \, c c c \)
calls the function like:
  (funcall #'a-func-of-MAC '(a) '(b b) '(c c c))
"
  (macrolet ((setup-macro (place)
               `(if-let ((entry (assoc name ,place
                                      :test #'preprocessor-macro-compare)))
                  (setf (cdr entry) val)
                  (push (cons name val) ,place))))
    (if for-upcase
        (setup-macro *preprocessor-macro-for-upcase*)
        (setup-macro *preprocessor-macro*))))
