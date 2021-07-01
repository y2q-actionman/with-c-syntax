(in-package #:cl-user)

(defpackage #:with-c-syntax.syntax
  (:use) ; Saying I use no packages explicitly. (If omitted, it is implementation-dependent.)
  (:export
   ;; operators
   #:\,
   #:= #:*= #:/= #:%= #:+= #:-= #:<<= #:>>= #:&= #:^= #:\|=
   #:? #:\:
   #:\|\|
   #:&&
   #:\|
   #:^
   #:&
   #:== #:!=
   #:< #:> #:<= #:>=
   #:>> #:<<
   #:+ #:-
   #:* #:/ #:%
   #:\( #:\)
   #:++ #:-- #:|sizeof|
   #:& #:* #:+ #:- #:~ #:!
   #:[ #:] #:\. #:->
   ;; keywords
   #:\;
   #:|auto| #:|register| #:|static| #:|extern| #:|typedef|
   #:|void| #:|char| #:|short| #:|int| #:|long|
   #:|float| #:|double| #:|signed| #:|unsigned|
   #:|const| #:|volatile|
   #:|struct| #:|union|
   #:|enum|
   #:|...|
   #:|case| #:|default|
   #:{ #:}
   #:|if| #:|else| #:|switch|
   #:|while| #:|do| #:|for|
   #:|goto| #:|continue| #:|break| #:|return|
   ;; extensions
   #:|__lisp_type| #:|__offsetof|)
  (:documentation
   "Holds symbols denoting C operators and keywords."))

(defpackage #:with-c-syntax.punctuator
  (:use #:with-c-syntax.syntax)
  (:export
   #:[ #:] #:\( #:\) #:{ #:} #:\. #:->
   #:++ #:-- #:& #:* #:+ #:- #:~ #:!
   #:/ #:% #:<< #:>> #:< #:> #:<= #:>= #:== #:!= #:^ #:\| #:&& #:\|\|
   #:? #:\: #:\; #:|...|
   #:= #:*= #:/= #:%= #:+= #:-= #:<<= #:>>= #:&= #:^= #:\|=
   #:\,
   ;; For preprocessor (their home package is just this package.)
   #:|#| #:|##|
   #:|<:| #:|:>| #:<% #:%> #:|%:| #:|%:%:|)
  (:documentation "C punctuators. Some symbols are from `with-c-syntax.syntax' package."))

(defpackage #:with-c-syntax.preprocessor-directive
  (:use)
  (:export
   #:|if| #:|ifdef| #:|ifndef| #:|elif| #:|else| #:|endif| #:|include|
   #:|define| #:|undef| #:|line| #:|error| #:|pragma|))

(defpackage #:with-c-syntax.core
  (:use #:cl #:with-c-syntax #:with-c-syntax.syntax)
  (:shadowing-import-from
   #:cl			      ; These CL symbols has same name with C.
   #:= #:/=  #:< #:> #:<= #:>=  #:+ #:- #:* #:/  #:++)
  (:use #:alexandria)
  (:import-from #:yacc
        	#:define-parser
                #:parse-with-lexer
		#:yacc-parse-error)
  (:import-from #:named-readtables
                #:defreadtable #:find-readtable)
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
   #:find-preprocessor-macro
   #:define-preprocessor-constant
   #:define-preprocessor-function
   #:preprocessor
   ;; pseudo-pointer.lisp
   #:pseudo-pointer
   #:with-pseudo-pointer-scope
   #:invalidate-all-pseudo-pointers
   #:pseudo-pointer-pointable-p
   #:make-pseudo-pointer
   #:pseudo-pointer-dereference
   #:pseudo-pointer-invalidate
   ;; reader.lisp
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
