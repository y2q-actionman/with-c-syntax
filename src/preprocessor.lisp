(in-package #:with-c-syntax.core)

(defun preprocessor-initial-set ()
  "This function returns the initial value of *preprocessor-macro*"
  (loop for sym in (append +operators+ +keywords+)
     as name = (symbol-name sym)
     as ucase = (string-upcase name)
     as macro = sym
     collect `((,name nil) . ,macro)
     when (string/= name ucase)
     collect `((,ucase :upcase) . ,macro)))

(defparameter *preprocessor-macro*
  (preprocessor-initial-set)
  "* Value Type
a vector of alist.
The alist is :: (<symbol or string> case-spec)
                 -> <function, or any literal object>.
 
* Description
Holds preprocessor macro definitions.

~case-spec~ is one of the following:
nil     :: means this macro definision is all readtable-case.
:upcase :: means this macro definision is only for the :upcase of
           readtable-case.
")

(defun find-preprocessor-macro (name case-spec)
  "* Syntax
~find-preprocessor-macro~ name &optional case-spec => macro-def

* Arguments and Values
- name      :: a string or symbol.
- case-spec :: ~:upcase~, or nil.
- macro-def :: a cons, which have ~name~ as its car, and macro
               definition as its cdr.

* Description
Finds and returns a preprocessor macro definition named ~name~.

~case-spec~ specifies for which case the macro is defined.  If
~:upcase~ is specified, the macro is defined only for the ~:upcase~
reader. If nil, the macro is defined for all readtable-case.

** Matching rule
- If ~name~ is a string, the matching functions is 'string='. Packages
are ignored.

- If ~name~ is a symbol, the matching functions is 'eq'. This is
package-aware.
"
  (loop for entry in *preprocessor-macro*
     as (e-name e-case) = (car entry)
     when (and (or (eq e-case nil)
		   (eq e-case case-spec))
	       (if (and (symbolp e-name) (symbolp name))
		   (eq e-name name)
		   (string= e-name name)))
     return entry))

(defun add-preprocessor-macro (name val &optional (case-spec nil))
  "* Syntax
~add-preprocessor-macro~ name val &optional upcase-spec => macro-def

* Arguments and Values
- name      :: a string or symbol.
- val       :: an object.
- case-spec :: ~:upcase~, or nil.
- macro-def :: a cons, which have ~name~ as its car, and macro
               definition as its cdr.

* Description
Establishes a new preprocessor macro.  When the preprocessor finds a
symbol matching ~name~, it is expanded by ~val~.

~case-spec~ specifies for which case the macro is defined.  If
~:upcase~ is specified, the macro is defined only for the ~:upcase~
reader. If nil, the macro is defined for all readtable-case.

* See Also
~find-preprocessor-macro~.
"
  (if-let ((entry (find-preprocessor-macro name case-spec)))
    (progn (setf (cdr entry) val)
	   entry)
    (let ((entry `((,name ,case-spec) . ,val)))
      (push entry *preprocessor-macro*)
      entry)))

(defun remove-preprocessor-macro (name &optional (case-spec nil))
  "* Syntax
~remove-preprocessor-macro~ name &optional upcase-spec => macro-def

* Arguments and Values
- name      :: a string or symbol.
- case-spec :: ~:upcase~, or nil.
- macro-def :: a cons, which have ~name~ as its car, and macro
               definition as its cdr.

* Description
Removes a preprocessor macro named ~name~.

~case-spec~ specifies for which case the macro is defined.  If
~:upcase~ is specified, the macro is defined only for the ~:upcase~
reader. If nil, the macro is defined for all readtable-case.

* See Also
~find-preprocessor-macro~.
"
  (when-let (entry (find-preprocessor-macro name case-spec))
    (deletef *preprocessor-macro* entry :test #'eq)
    entry))

(defun preprocessor-call-macro (lis-head fn)
  "A part of the ~preprocessor~ function."
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

(defun preprocessor (lis &optional (case-spec nil))
  "* Syntax
~preprocessor~ list-of-tokens &key allow-upcase-keyword => preprocesed-list

* Arguments and Values
- list-of-tokens       :: a list
- case-spec            :: ~:upcase~, or nil.
- preprocesed-list     :: a list

* Description
This function works like the C Preprocessor.

** Working.
At this stage, all of the functionalities of the standard CPP are not
implemented. Current working is below:

- Concatenation of string literals.

- Calling preprocessor macro (not recursive)
  Invoking preprocessor macros, defined by add-preprocessor-macro.

  This system is used for converting symbols denoting keywords of C
  belongs other packages to our package's one.
  
- A dirty hack for 'typedef'.
  Autimatically addes 'void ;' after each typedef declarations.  This
  is a workaround for avoiding the problem between 'the lexer hack'
  and the look-aheading of cl-yacc.

~case-spec~ specifies which macro definitions are used. If ~:upcase~
is specified, macro definitions for ~:upcase~ reader is used
additionally.

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

* See Also
~find-preprocessor-macro~.

* Notes
- TODO: recursive expansion
"
  (loop with ret = nil
     with typedef-hack = nil
     for token = (pop lis)
     while token
     ;; preprocessor macro
     when (symbolp token)
     do (when-let*
            ((entry (find-preprocessor-macro token case-spec))
             (expansion (cdr entry)))
          (cond ((null expansion)     ; no-op
                 nil)                   
                ((functionp expansion) ; preprocessor funcion
                 (multiple-value-bind (ex-val new-lis)
                     (preprocessor-call-macro lis expansion)
                   (push ex-val ret)
                   (setf lis new-lis)
                   (setf token nil)))
                (t                  ; symbol expansion
                 (push expansion ret)
                 (setf token nil))))
     ;; string concatenation
     when (stringp token)
     do (loop for i = (pop lis)
           while (stringp i)
	   collect i into strs
           finally
	     (push i lis)
             (push (apply #'concatenate 'string token strs) ret)
             (setf token nil))
     ;; otherwise..
     when token
     do (push token ret)

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
