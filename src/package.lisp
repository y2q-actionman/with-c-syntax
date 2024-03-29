(in-package #:cl-user)

;;; Packages for C syntax.

(defpackage #:with-c-syntax.syntax
  (:use) ; Saying I use no packages explicitly. (If omitted, it is implementation-dependent.)
  (:export
   ;; operators
   ","
   "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
   "?" ":"
   "||"
   "&&"
   "|"
   "^"
   "&"
   "==" "!="
   "<" ">" "<=" ">="
   ">>" "<<"
   "+" "-"
   "*" "/" "%"
   "(" ")"
   "++" "--" "sizeof"
   "&" "*" "+" "-" "~" "!"
   "[" "]" "." "->"
   ;; keywords
   ";"
   "auto" "register" "static" "extern" "typedef"
   "void" "char" "short" "int" "long"
   "float" "double" "signed" "unsigned"
   "const" "volatile"
   "struct" "union"
   "enum"
   "..."
   "case" "default"
   "{" "}"
   "if" "else" "switch"
   "while" "do" "for"
   "goto" "continue" "break" "return"
   ;; extensions
   "__lisp_type" "__offsetof")
  (:documentation
   "Holds symbols denoting C operators and keywords."))

(defpackage #:with-c-syntax.punctuator
  (:use)
  (:import-from #:with-c-syntax.syntax
                "[" "]" "(" ")" "{" "}" "." "->"
                "++" "--" "&" "*" "+" "-" "~" "!"
                "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
                "?" ":" ";" "..."
                "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
                ",")
  (:export
   "[" "]" "(" ")" "{" "}" "." "->"
   "++" "--" "&" "*" "+" "-" "~" "!"
   "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
   "?" ":" ";" "..."
   "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
   ","
   ;; For preprocessor (their home package is just this package.)
   "#" "##"
   "<:" ":>" "<%" "%>" "%:" "%:%:"
   ;; For processing const-exp
   "__pp_const_exp")
  (:documentation "C punctuators. Some symbols are from `with-c-syntax.syntax' package."))

(defpackage #:with-c-syntax.preprocessor-directive
  (:use)
  (:export "if" "ifdef" "ifndef" "elif" "else" "endif" "include"
           "define" "undef" "line" "error" "pragma"))

(defpackage #:with-c-syntax.preprocess-operator
  (:use)
  (:export "defined" "_Pragma"))

(defpackage #:with-c-syntax.pragma-name
  (:use)
  (:export "STDC" "WITH_C_SYNTAX" "once"))

(defpackage #:with-c-syntax.predefined-macro
  (:use)
  (:export "__DATE__" "__FILE__" "__LINE__" "__STDC__"
           "__STDC_HOSTED__" "__STDC_MB_MIGHT_NEQ_WC__"
           "__STDC_VERSION__" "__TIME__"))

;;; Package for Lisp

(defpackage #:with-c-syntax.core
  (:use #:cl #:with-c-syntax #:with-c-syntax.syntax)
  (:shadowing-import-from
   #:cl              ; Some CL symbols has same name with C operators.
   #:= #:/= #:< #:> #:<= #:>= #:+ #:- #:* #:/ #:++)
  (:use #:alexandria)
  (:import-from #:yacc
        	#:define-parser
                #:parse-with-lexer
		#:yacc-parse-error)
  (:use #:named-readtables)
  (:export
   ;; util.lisp
   #:+nul-character+
   #:+vertical-tab-character+
   #:+bel-character+
   #:c-whitespace-p
   ;; condition.lisp
   #:with-c-syntax-error
   #:with-c-syntax-warning
   #:with-c-syntax-style-warning
   ;; preprocessor.lisp
   #:*with-c-syntax-find-include-file-function-list*
   #:find-asdf-system-relative-file
   #:preprocessor
   #:preprocessing-number
   #:preprocessing-number-p
   #:parse-preprocessing-number
   #:preprocessing-number-string
   ;; pseudo-pointer.lisp
   #:pseudo-pointer
   #:with-pseudo-pointer-scope
   #:invalidate-all-pseudo-pointers
   #:pseudo-pointer-pointable-p
   #:make-pseudo-pointer
   #:pseudo-pointer-dereference
   #:pseudo-pointer-invalidate
   ;; reader.lisp
   #:*previous-readtable*
   #:+newline-marker+                   ; for test code.
   #:+whitespace-marker+                ; for test code.
   #:with-c-syntax-readtable
   #:*with-c-syntax-reader-level*
   #:*with-c-syntax-reader-case*
   ;; struct.lisp
   #:find-struct-spec
   #:add-struct-spec
   #:remove-struct-spec
   #:struct
   #:make-struct
   #:struct-member
   ;; typedef.lisp
   #:find-typedef
   #:add-typedef
   #:remove-typedef
   ;; with-c-syntax.lisp
   #:enum
   #:get-variadic-arguments
   #:with-c-syntax)
  (:documentation
   "with-c-syntax core package."))
