(in-package #:with-c-syntax.core)

(defmacro define-case-aware-find-symbol
    (finder-function-name package-name
     &key (upcased-package-name (format nil "~A.~A" (string package-name) '#:UPCASED))
       (docstring nil))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpackage ,upcased-package-name
       (:use)
       (:intern
        ,@(loop for sym being the external-symbol of package-name
                collect (string-upcase (symbol-name sym)))))
     ,@(loop for sym being the external-symbol of package-name
             for upcased = (string-upcase (symbol-name sym))
             collect `(setf (symbol-value (find-symbol ,upcased ',upcased-package-name))
                            ',sym))
     (defun ,finder-function-name (name readtable-case)
       ,@(if docstring `(,docstring))
       (ecase readtable-case
         ((:upcase :invert)
          (if-let ((up-sym (find-symbol name ',upcased-package-name)))
            (symbol-value up-sym)))
         ((:downcase :preserve)
          (find-symbol name ',package-name))))))

;;; C syntax words

(define-case-aware-find-symbol find-c-terminal
  #:with-c-syntax.syntax
  :docstring
  "Find a symbol in `with-c-syntax.syntax' package having a same
NAME. If not found, returns `nil'.")

;;; CPP directives

(define-case-aware-find-symbol find-preprocessor-directive
  #:with-c-syntax.preprocessor-directive
  :docstring
  "Find a symbol in `with-c-syntax.preprocessor-directive' package
having a same NAME. If not found, returns `nil'.")

;;; C punctuators.

(defun find-punctuator (name process-digraph?)
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
(defun pp-|defined|-operator-p (token readtable-case)
  (ecase readtable-case
    ((:upcase :invert) (string= token "DEFINED"))
    ((:downcase :preserve) (string= token "defined"))))

(defun pp-if-expression-lexer (token-list process-digraph? readtable-case directive-symbol
                               pp-state-macro-alist)
  #'(lambda ()
      (if
       (null token-list)
       (values nil nil)
       (let ((token (pop-preprocessor-directive-token token-list directive-symbol :errorp nil)))
         (typecase token
           (null
            (values 'lisp-expression nil))
           (symbol
            (cond
              ((when-let (punctuator (find-punctuator (symbol-name token) process-digraph?))
                 (values punctuator punctuator)))
              ((pp-|defined|-operator-p token readtable-case)
               ;; defined operator
               (let* ((defined-1 (pop-preprocessor-directive-token token-list directive-symbol))
                      (param
                        (if (and (symbolp defined-1)
                                 (string= defined-1 "("))
                            (prog1 (pop-preprocessor-directive-token token-list directive-symbol)
                              (let ((r-paren? (pop-preprocessor-directive-token
                                               token-list directive-symbol)))
                                (unless (and (symbolp r-paren?)
                                             (string= r-paren? ")"))
                                  (error
                                   'preprocess-error
	                           :format-control "'defined' operator does not have corresponding ')'. '~A' was found."
	                           :format-arguments (list r-paren?)))))
                            defined-1)))
                 (when (or (not (symbolp param))
                           (find-punctuator (symbol-name param) process-digraph?))
                   (error
                    'preprocess-error
	            :format-control "'defined' operator takes only identifiers. '~A' was passed."
	            :format-arguments (list param)))
                 (values 'lisp-expression
                         (if (preprocessor-macro-exists-p pp-state-macro-alist param) t nil))))
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
           (otherwise
            (values 'lisp-expression token)))))))

;;; Lexer for compiler.
(defun list-lexer (list readtable-case
                   &aux (syntax-package (find-package '#:with-c-syntax.syntax))
                     (typedef-hack? nil))
  ;; # This lexer does a dirty hack for 'typedef'.
  ;; It automatically adds 'void ;' after each typedef declarations.
  ;; This is a workaround for avoiding the problem between 'the lexer
  ;; hack' and the look-aheading of cl-yacc.
  #'(lambda ()
      (let ((value (pop list)))
        (typecase value
          (null
           (if list
               (values 'id nil)
               (values nil nil)))
          (symbol
           (mv-cond-let (cond-var)
             ((find-c-terminal (symbol-name value) readtable-case)
              ;; typedef hack -- adds "void ;" after each typedef.
              (case cond-var
                (|typedef|
                 (setf typedef-hack? t))
                (\;
                 (when typedef-hack?
                   (setf typedef-hack? nil)
                   (setf list (list* (ecase readtable-case
                                       ((:upcase :invert) '|VOID|)
                                       ((:downcase :preserve) '|void|))
                                     '\; list)))))
              (values cond-var cond-var))
             ;; FIXME: this part is should be removed, but I need it now
             ;; for some tests..
             ((eql (symbol-package value) syntax-package)
              ;; typedef hack -- adds "void ;" after each typedef.
              (case value
                (|typedef|
                 (setf typedef-hack? t))
                (\;
                 (when typedef-hack?
                   (setf typedef-hack? nil)
                   (setf list (list* '|void| '\; list)))))
              (values value value))
             ((gethash value *typedef-names*)
              (values 'typedef-id value))
             (t
              (values 'id value))))
          (integer
           (values 'int-const value))
          (character
           (values 'char-const value))
          (float
           (values 'float-const value))
          (string
           (values 'string value))
          (otherwise
           (values 'lisp-expression value))))))
