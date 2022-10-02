(in-package #:with-c-syntax.core)

;;; C punctuators.

(defvar *with-c-syntax-preprocessor-process-digraph* nil
  "Determines whether preprocessor replaces digraphs.
 If this is true, replacement occurs but `with-c-syntax-style-warning' is signalled.
 If this is `:no-warn', replacement occurs and the style-warning is not signalled.")

(defun find-punctuator (name
                        &optional (process-digraph? *with-c-syntax-preprocessor-process-digraph*))
  (when-let (punctuator (find-symbol name '#:with-c-syntax.punctuator))
    (when-let (replace (find-digraph-replacement punctuator)) ; Check digraph
      (unless (eq process-digraph? :no-warn)
        (warn 'with-c-syntax-style-warning
              :message (format nil "Digraph sequence '~A' (means ~A) found."
                               punctuator replace)))
      (when process-digraph?
        (setf punctuator replace)))
    punctuator))

(defun find-digraph-replacement (punctuator)
  (case punctuator
    (with-c-syntax.punctuator:|<:| 'with-c-syntax.syntax:[)
    (with-c-syntax.punctuator:|:>| 'with-c-syntax.syntax:])
    (with-c-syntax.punctuator:|<%| 'with-c-syntax.syntax:{)
    (with-c-syntax.punctuator:|%>| 'with-c-syntax.syntax:})
    (with-c-syntax.punctuator:|%:| 'with-c-syntax.punctuator:|#|)
    (with-c-syntax.punctuator:|%:%:| 'with-c-syntax.punctuator:|##|)))

;;; Lexer for '#if' preprocessor-directive.

(defun pp-if-expression-lexer (token-list)
  #'(lambda ()
      (if
       (null token-list)
       (values nil nil)
       (let ((token (pop token-list)))
         (typecase token
           (null
            (values 'lisp-expression nil))
           (symbol
            (cond-let (it)
              ((find-punctuator (symbol-name token))
               (values it it))
              (t
               ;; In C99, remaining identifiers are replaced to 0.
               ;; ("6.10.1 Conditional inclusion" in ISO/IEC 9899.)
               ;; I replace it to `cl:nil' for compromising Lisp manner.
               ;; This substitution will supress casts.
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
  ;; # This lexer has a dirty hack for 'typedef'.
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
            (values 'lisp-expression nil))
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
