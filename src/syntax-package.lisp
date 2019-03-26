(in-package :cl-user)

(defpackage #:with-c-syntax.syntax
  (:use :cl)
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
