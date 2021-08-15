(in-package #:with-c-syntax.core)

;;; Lexer for '#if' preprocessor-directive.

(defun pp-if-expression-lexer (token-list process-digraph?)
  #'(lambda ()
      (if
       (null token-list)
       (values nil nil)
       (let ((token (pop token-list)))
         (typecase token
           (null
            (values 'lisp-expression nil))
           (symbol
            (mv-cond-let (cond-var)
              ((find-punctuator (symbol-name token) process-digraph?)
               (values cond-var cond-var))
              (t
               ;; In C99, remaining identifiers are replaced to 0.
               ;; ("6.10.1 Conditional inclusion" in ISO/IEC 9899.)
               ;; I replace it to `cl:nil' for compromising Lisp manner.
               ;; This substitutuin will supress casts.
               (values 'lisp-expression nil))))
           (integer
            (values 'int-const token))
           (character
            (values 'char-const token))
           (float
            (values 'float-const token)) ; FIXME: #if does not accept floats.
           (string
            (values 'string token))
           (preprocessing-number
            (let ((num (parse-preprocessing-number token)))
              (etypecase num
                (integer (values 'int-const num))
                (float (values 'float-const num)))))
           (otherwise
            (values 'lisp-expression token)))))))

;;; Lexer for compiler.
(defun list-lexer (token-list
                   &aux (syntax-package (find-package '#:with-c-syntax.syntax))
                     (typedef-hack? nil))
  ;; # This lexer does a dirty hack for 'typedef'.
  ;; It automatically adds 'void ;' after each typedef declarations.
  ;; This is a workaround for avoiding the problem between 'the lexer
  ;; hack' and the look-aheading of cl-yacc.
  #'(lambda ()
      (if
       (null token-list)
       (values nil nil)
       (let ((token (pop token-list)))
         (typecase token
           (null
            (values 'id nil))
           (symbol
            (cond
              ((eql (symbol-package token) syntax-package)
               ;; typedef hack -- adds "void ;" after each typedef.
               (case token
                 (|typedef|
                  (setf typedef-hack? t))
                 (\;
                  (when typedef-hack?
                    (setf typedef-hack? nil)
                    (setf token-list (list* '|void| '\; token-list)))))
               (values token token))
              ((gethash token *typedef-names*)
               (values 'typedef-id token))
              (t
               (values 'id token))))
           (integer
            (values 'int-const token))
           (character
            (values 'char-const token))
           (float
            (values 'float-const token))
           (string
            (values 'string token))
           (preprocessing-number
            (let ((num (parse-preprocessing-number token)))
              (etypecase num
                (integer (values 'int-const num))
                (float (values 'float-const num)))))
           (otherwise
            (values 'lisp-expression token)))))))
