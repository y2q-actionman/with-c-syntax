(in-package :cl-user)

(defpackage #:with-c-syntax.syntax
  (:import-from #:cl
		#:= #:/=
		#:< #:> #:<= #:>=
		#:+ #:- #:* #:/
		#:++)
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
   "Holds symbols denoting C operators and keywords. "))

(defpackage #:with-c-syntax.syntax-upcase
  (:export ; TODO: move them into internal symbols of `with-c-syntax.syntax'.
   #:|SIZEOF|
   #:|AUTO| #:|REGISTER| #:|STATIC| #:|EXTERN| #:|TYPEDEF|
   #:|VOID| #:|CHAR| #:|SHORT| #:|INT| #:|LONG|
   #:|FLOAT| #:|DOUBLE| #:|SIGNED| #:|UNSIGNED|
   #:|CONST| #:|VOLATILE|
   #:|STRUCT| #:|UNION|
   #:|ENUM|
   #:|CASE| #:|DEFAULT|
   #:|IF| #:|ELSE| #:|SWITCH|
   #:|WHILE| #:|DO| #:|FOR|
   #:|GOTO| #:|CONTINUE| #:|BREAK| #:|RETURN|
   #:|__LISP_TYPE| #:|__OFFSETOF|)
  (:documentation
   "Holds symbols denoting C operators and keywords. "))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (usym (find-package '#:with-c-syntax.syntax-upcase))
    (setf (symbol-value usym)
	  (find-symbol (string-downcase (symbol-name usym))
		       (find-package '#:with-c-syntax.syntax)))))
