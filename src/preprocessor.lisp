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

(defvar *with-c-syntax-preprocessor-process-digraph* nil
  "Determines whether preprocessor replaces digraphs.
 If this is true, replacement occurs but `with-c-syntax-style-warning' is signalled.
 If this is `:no-warn', replacement occurs and the style-warning is not signalled.")

;;; Preprocessor macro.

(defpackage #:with-c-syntax.preprocessor-special-macro
  (:use)
  (:export #:__DATE__ #:__FILE__ #:__LINE__ #:__TIME__))

(defconstant +preprocessor-macro+
  '+preprocessor-macro+
  "A symbol used as an indicator of `symbol-plist' holding the preprocessor function.
See `add-preprocessor-macro'.")

(defun preprocessor-macro-exists-p (symbol)
  (or (find-symbol (symbol-name symbol) '#:with-c-syntax.preprocessor-special-macro)
      (get-properties (symbol-plist symbol) `(,+preprocessor-macro+))))

(defun find-preprocessor-macro (symbol)
  "Finds and returns a preprocessor macro definition named SYMBOL,
added by `add-preprocessor-macro'."
  (or (find-symbol (symbol-name symbol) '#:with-c-syntax.preprocessor-special-macro)
      (get symbol +preprocessor-macro+)))

(defun add-preprocessor-macro (symbol value)
  "Establishes a new preprocessor macro to SYMBOL, which is corresponded to VALUE.

Preprocessor macros are held in `symbol-plist' of SYMBOL, and
indicated by `+preprocessor-macro+'."
  (setf (get symbol +preprocessor-macro+) value))

(defun remove-preprocessor-macro (symbol)
  "Removes a preprocessor macro named SYMBOL."
  (remprop symbol +preprocessor-macro+))

(defmacro define-preprocessor-constant (name value &optional documentation)
  "Defines a new preprocessor symbol macro, named by NAME and its value is VALUE."
  `(progn
     (define-constant ,name ,value
       :test 'equal
       :documentation ,documentation)
     (add-preprocessor-macro ',name ,value)))

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
         ;; TODO: use `pop-preprocessor-directive-token' for treating `+whitespace-marker+'.
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

;;; Identifier split.

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

(defclass preprocessor-state () 
  ((readtable-case :initarg :readtable-case :initform :upcase :type keyword
                   :reader pp-state-readtable-case)
   (process-digraph? :initarg :process-digraph? :initform nil :type boolean
                     :reader pp-state-process-digraph?)
   (file-pathname :initarg :file-pathname :initform nil
                  :accessor pp-state-file-pathname)
   (token-list :initarg :token-list :type list
               :accessor pp-state-token-list)
   (result-list :initform nil :type list
                :accessor pp-state-result-list)
   (line-number :initform 1 :type integer
                :accessor pp-state-line-number)
   (tokens-in-line :initform 0 :type integer
                   :accessor pp-state-tokens-in-line)
   (if-section-stack :initform nil :type list
                     :accessor pp-state-if-section-stack)
   (if-section-skip-reason :initform nil
                           :accessor pp-state-if-section-skip-reason)
   (include-stack :initform nil
                  :accessor pp-state-include-stack)))

;; TODO: reduce its usage
(defmacro with-preprocessor-state-slots ((state) &body body)
  `(with-accessors ((readtable-case pp-state-readtable-case)
                    (process-digraph? pp-state-process-digraph?)
                    (file-pathname pp-state-file-pathname)
                    (token-list pp-state-token-list)
                    (result-list pp-state-result-list)
                    (line-number pp-state-line-number)
                    (tokens-in-line pp-state-tokens-in-line)
                    (if-section-stack pp-state-if-section-stack)
                    (if-section-skip-reason pp-state-if-section-skip-reason)
                    (include-stack pp-state-include-stack))
       ,state
     ,@body))

(defun draw-preprocessor-directive-line-tokens (state)
  (with-preprocessor-state-slots (state)
    (loop for i = (first token-list)
          while token-list
          until (eq i +newline-marker+)
          do (pop token-list)
          collect i)))

(defun raise-no-preprocessor-token-error (directive-name)
  (error 'preprocess-error
         :format-control "No token after #~A"
         :format-arguments (list directive-name)))

(defmacro check-and-pop-pp-directive-token-1 (token-list directive-name errorp)
  "Used by `pop-preprocessor-directive-token'"
  `(if (null ,token-list)
       ,(if errorp
            `(raise-no-preprocessor-token-error ,directive-name)
            nil)
       (pop ,token-list)))

(defmacro pop-preprocessor-directive-token (token-list directive-name &key (errorp t))
  "Pops the next token in TOKEN-LIST ignoring `+whitespace-marker+'"
  (with-gensyms (head_)
    `(let ((,head_
             (check-and-pop-pp-directive-token-1 ,token-list ,directive-name ,errorp)))
       (if (eq +whitespace-marker+ ,head_)
           (check-and-pop-pp-directive-token-1 ,token-list ,directive-name ,errorp)
           ,head_))))

(defun preprocessor-token-exists-p (token-list)
  (loop for token in token-list
        thereis (not (eq token +whitespace-marker+))))

(defun check-no-preprocessor-token (token-list directive-name)
  "Checks no preprocessing token is left in TOKEN-LIST."
  (when (preprocessor-token-exists-p token-list)
    (error 'preprocess-error
           :format-control "Extra tokens are found after #~A"
           :format-arguments (list (string directive-name)))))

(defmacro pop-last-preprocessor-directive-token (token-list directive-name)
  "For #ifdef, #ifndef, and #undef"
  (with-gensyms (dsym_)
    `(let ((,dsym_ ,directive-name))
       (prog1 (pop-preprocessor-directive-token ,token-list ,dsym_)
         (check-no-preprocessor-token ,token-list ,dsym_)))))

(defgeneric process-preprocessing-directive (directive-symbol token-list state))

(defclass if-section ()
  ((group-conditions :initform (make-array 1 :fill-pointer 0 :adjustable t)
                      :type vector)))

(defmethod print-object ((obj if-section) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defmethod if-section-add-group (if-section form result)
  (with-slots (group-conditions) if-section
    (vector-push-extend (cons form result) group-conditions)))

(defmethod if-section-processed-p (if-section)
  (with-slots (group-conditions) if-section
    (loop with processed? = nil
          for (condition . eval-result) across group-conditions
          when (eq condition :else)
            do (error 'preprocess-error
                      :format-control "#else already appeared.")
          when eval-result
            do (setf processed? t)
          finally (return processed?))))

(defun begin-if-section (state condition-form eval-result)
  (let ((if-section-obj (make-instance 'if-section)))
    (if-section-add-group if-section-obj condition-form eval-result)
    (with-preprocessor-state-slots (state)
      (push if-section-obj if-section-stack)
      (when (and (null if-section-skip-reason)
                 (not eval-result))
        (setf if-section-skip-reason if-section-obj)))))

(defun pp-|defined|-operator-p (token readtable-case)
  (ecase readtable-case
    ((:upcase :invert) (string= token "DEFINED"))
    ((:downcase :preserve) (string= token "defined"))))

(defun pp-if-expression-lexer (token-list process-digraph? readtable-case directive-symbol)
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
                         (preprocessor-macro-exists-p param))))
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

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|if|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (begin-if-section state '(:skipped :if) nil)
      (return-from process-preprocessing-directive nil))
    (unless (preprocessor-token-exists-p directive-token-list)
      (raise-no-preprocessor-token-error directive-symbol))
    (let* ((condition directive-token-list)
           ;; TODO: Expand PP macro before parsing.
           (lexer (pp-if-expression-lexer directive-token-list process-digraph? readtable-case
                                          directive-symbol))
           (parsed-form
             (handler-case
                 (with-c-compilation-unit (nil t)
                   (parse-with-lexer lexer *expression-parser*))
               (yacc-parse-error (condition)
                 (error 'preprocess-if-expression-parse-error :yacc-error condition))))
           (check-left-token
             (when (funcall lexer)
               (error 'preprocess-error
                      :format-control "Extra tokens are found after #if")))
           (value (eval parsed-form))
           (result (and value
                        (not (eql value 0))))) ; Special case for "#if 0" idiom.
      (declare (ignore check-left-token))
      (begin-if-section state condition result))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|ifdef|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (begin-if-section state '(:skipped :ifdef) nil)
      (return-from process-preprocessing-directive nil))
    (let* ((identifier
             (pop-last-preprocessor-directive-token directive-token-list directive-symbol))
           (condition `(:ifdef ,identifier))
           (result (preprocessor-macro-exists-p identifier)))
      (begin-if-section state condition result))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|ifndef|))
                                            directive-token-list state)
  ;; TODO: merge with 'ifdef'.
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (begin-if-section state '(:skipped :ifndef) nil)
      (return-from process-preprocessing-directive nil))
    (let* ((identifier
             (pop-last-preprocessor-directive-token directive-token-list directive-symbol))
           (condition `(:ifndef ,identifier))
           (result (not (preprocessor-macro-exists-p identifier))))
      (begin-if-section state condition result))))

(defun check-in-if-section (state directive-symbol)
  (with-preprocessor-state-slots (state)
    (unless if-section-stack
      (error 'preprocess-error
             :format-control "#~A appeared outside of #if section."
             :format-arguments (list (symbol-name directive-symbol))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|elif|))
                                            directive-token-list state)
  (check-in-if-section state directive-symbol)
  (with-preprocessor-state-slots (state)
    (let* ((current-if-section (first if-section-stack))
           (if-section-processed-p (if-section-processed-p current-if-section)))
      (cond
        ((and if-section-skip-reason
              (not (eq if-section-skip-reason current-if-section))) ; Skipped because of outer if-section.
         (if-section-add-group current-if-section '(:skipped :elif) nil))
        (if-section-processed-p
         (if-section-add-group current-if-section '(:processed :elif) nil)
         (setf if-section-skip-reason current-if-section))
        (t
         ;; TODO: merge with #if part.
         (unless (preprocessor-token-exists-p directive-token-list)
           (raise-no-preprocessor-token-error directive-symbol))
         (let* ((condition directive-token-list)
                (lexer (pp-if-expression-lexer directive-token-list process-digraph? readtable-case
                                               directive-symbol))
                (parsed-form
                  (handler-case
                      (with-c-compilation-unit (nil t)
                        (parse-with-lexer lexer *expression-parser*))
                    (yacc-parse-error (condition)
                      (error 'preprocess-if-expression-parse-error :yacc-error condition))))
                (check-left-token
                  (when (funcall lexer)
                    (error 'preprocess-error
                           :format-control "Extra tokens are found after #elif")))
                (value (eval parsed-form))
                (result (and value
                             (not (eql value 0))))) ; Special case for "#if 0" idiom.
           (declare (ignore check-left-token))
           (if-section-add-group current-if-section condition result)
           (setf if-section-skip-reason (if result
                                            nil
                                            current-if-section))))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|else|))
                                            directive-token-list state)
  (check-no-preprocessor-token directive-token-list directive-symbol)
  (check-in-if-section state directive-symbol)
  (with-preprocessor-state-slots (state)
    (let* ((current-if-section (first if-section-stack))
           (if-section-processed-p (if-section-processed-p current-if-section)))
      (cond
        ((and if-section-skip-reason
              (not (eq if-section-skip-reason current-if-section))) ; Skipped because of outer if-section.
         (if-section-add-group current-if-section '(:skipped :else) nil))
        (if-section-processed-p
         (if-section-add-group current-if-section :else nil)
         (setf if-section-skip-reason current-if-section))
        (t
         (if-section-add-group current-if-section :else t)
         (setf if-section-skip-reason nil))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|endif|))
                                            directive-token-list state)
  (check-no-preprocessor-token directive-token-list directive-symbol)
  (check-in-if-section state directive-symbol)
  (with-preprocessor-state-slots (state)
    (let ((removed-if-section (pop if-section-stack)))
      (when (eq if-section-skip-reason removed-if-section)
        (setf if-section-skip-reason nil)))))

(defun parse-header-name (token-list directive-symbol &key (try-pp-macro-expand t))
  ;; FIXME: Current header-name implementation does not recognize implementation-defined points.
  (let ((token1 (pop-preprocessor-directive-token token-list directive-symbol)))
    (cond
      ((stringp token1)
       ;; FIXME: Treating of these chars is implementation-defined: ', \, //, or /* .
       ;; See ISO/IEC 9899:1999, page 64.
       (check-no-preprocessor-token token-list directive-symbol)
       (values token1 :q-char-sequence))
      ((and (symbolp token1) (string= token1 "<"))
       ;; FIXME: Treating of these chars is implementation-defined: ', \, ", //, or /* .
       ;; See ISO/IEC 9899:1999, page 64.
       ;; FIXME: The number of whitespaces is not preserved. To fix it,
       ;;  I must treat whitespaces specially only after '#include' by our reader.
       (loop for token = (pop token-list)
             until (and (symbolp token) (string= token ">"))
             collect
             (cond ((eq token +whitespace-marker+)
                    #\space)
                   ((symbolp token)
                    (symbol-name token))
                   (t
                    token))
               into h-tokens
             finally
                (check-no-preprocessor-token token-list directive-symbol)
                (return (values (format nil "~{~A~}" h-tokens)
                                :h-char-sequence))))
      (try-pp-macro-expand
       ;; FIXME: macroexpand here
       (return-from parse-header-name
         (parse-header-name token-list directive-symbol :try-pp-macro-expand nil)))
      (t
       (error 'preprocess-error
              :format-control "'~A' cannot be used for #~A"
              :format-arguments (list token1 directive-symbol))))))

(defclass saved-include-state ()
  ((if-section-stack :initarg :if-section-stack
                     :reader saved-include-state-if-section-stack)))

(defmethod push-include-state ((state preprocessor-state))
  (with-preprocessor-state-slots (state)
    (let ((i-state (make-instance 'saved-include-state
                                  :if-section-stack if-section-stack)))
      (push i-state include-stack))))

(defmethod pop-include-state ((state preprocessor-state))
  (with-preprocessor-state-slots (state)
    (let ((i-state (pop include-stack)))
      (unless (eql if-section-stack
                   (saved-include-state-if-section-stack i-state))
        (error 'preprocess-error
               :format-control "#if section does not end in included file ~A"
               :format-arguments (list file-pathname))))))

(define-constant +with-c-syntax-pragma+ "WCS"
  :test 'equal)

(define-constant +end-of-inclusion-subdirective+ "END_OF_INCLUSION"
  :test 'equal)

(defun find-include-<header>-file (header-name &key (errorp t))
  "Finds a file specified by #include <...> style header-name.
 Current strategy is only looking with-c-syntax specific files."
  (let* ((wcs-include-directory
           (asdf:system-relative-pathname :with-c-syntax "include"
                                          :type :directory))
         (path (merge-pathnames header-name wcs-include-directory)))
    (or (probe-file path)
        (when errorp
          (error 'preprocess-include-file-error
                 :pathname header-name)))))

(defun find-include-header-file (header-name &key (errorp t))
  "Finds a file specified by #include \"...\" style header-name.
 Current strategy is just to use `cl:probe-file'. So it will affected
 by `*default-pathname-defaults*'.
 If no file was found, `find-include-<header>-file' is called."
  (or (probe-file header-name)
      (find-include-<header>-file header-name :errorp errorp)))

(defun tokenize-included-source-file (header-name state reader-level)
  (let ((start-pragma-string
          ;; TODO: use tokens
          (format nil "#line 1 \"~A\"~%" header-name))
        (end-pragma-string
          (with-output-to-string (out)
            (terpri out)
            (format out "#pragma ~A ~A~%"
                    +with-c-syntax-pragma+ +end-of-inclusion-subdirective+)
            (format out "#line ~A \"~A\"~%"
                    (pp-state-line-number state) (pp-state-file-pathname state)))))
    (with-input-from-string (start-pragma start-pragma-string)
      (with-open-file (main-source header-name)
        (with-input-from-string (end-pragma end-pragma-string)
          (with-open-stream (stream (make-concatenated-stream start-pragma main-source end-pragma))
            (tokenize-source reader-level stream nil)))))))

(defmacro pop-next-newline (token-list)
  "Pop the direcive's newline for setting next line's line-number to same."
  `(let ((next-token (pop ,token-list)))
     (assert (eq next-token +newline-marker+))
     next-token))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|include|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (multiple-value-bind (header-name header-type)
        (parse-header-name directive-token-list directive-symbol)
      (let* ((pathname (ecase header-type
                         (:q-char-sequence (find-include-header-file header-name))
                         (:h-char-sequence (find-include-<header>-file header-name))))
             (included-tokens
               ;; TODO: FIXME: I should take the caller's reader level!
               (tokenize-included-source-file pathname state 2)))
        (push-include-state state)
        (pop-next-newline token-list)
        (setf token-list
              (nconc included-tokens token-list))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|define|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    ;; TODO: Add local macro scope for preprocessor macros.
    (let* ((identifier
             (pop-preprocessor-directive-token directive-token-list directive-symbol))
           (function-like-p (eql (first directive-token-list) 'with-c-syntax.punctuator:|(|)))
      (cond
        (function-like-p
         (error "TODO: function-like macro"))
        (t
         (add-preprocessor-macro identifier directive-token-list))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|undef|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (let ((identifier
            (pop-last-preprocessor-directive-token directive-token-list directive-symbol)))
      (remove-preprocessor-macro identifier))))

(defun parse-line-arguments (token-list directive-symbol &key (try-pp-macro-expand t))
  (let* ((tmp-token-list token-list)
         (line-number (pop-preprocessor-directive-token tmp-token-list directive-symbol))
         (file-name-supplied-p
           (preprocessor-token-exists-p tmp-token-list))
         (file-name
           (if file-name-supplied-p
               (pop-preprocessor-directive-token tmp-token-list directive-symbol))))
    (cond
      ((and (integerp line-number)
            ;; TODO: check the line number consists of digit-sequence only.
            (or (null file-name) (stringp file-name))
            (not (preprocessor-token-exists-p tmp-token-list)))
       (values line-number file-name file-name-supplied-p))
      (try-pp-macro-expand
       ;; TODO: expand macros here.
       (return-from parse-line-arguments
         (parse-line-arguments token-list directive-symbol :try-pp-macro-expand nil)))
      (t
       (error 'preprocess-error
              :format-control "#~A syntax error. Arguments are ~A"
              :format-arguments (list directive-symbol token-list))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|line|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (multiple-value-bind (arg-line-number arg-file-name arg-file-name-supplied-p)
        (parse-line-arguments directive-token-list directive-symbol)
      (unless (<= 1 arg-line-number 2147483647)
        (error 'preprocess-error
               :format-control "#~A line number '~A' is out of range."
               :format-arguments (list directive-symbol arg-line-number)))
      (setf line-number arg-line-number)
      (when arg-file-name-supplied-p
        (setf file-pathname (parse-namestring arg-file-name)))
      ;; Pop the direcive's newline for setting next line's line-number to same.
      (pop-next-newline token-list))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|error|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (let ((token-list-for-print (delete +whitespace-marker+ directive-token-list)))
      (error 'preprocess-error
             :format-control "[File ~A: Line ~D] #error: ~{~A~^ ~}"
             :format-arguments (list (if file-pathname (enough-namestring file-pathname)) 
                                     line-number
                                     token-list-for-print)))))

(defun process-with-c-syntax-pragma (directive-symbol directive-token-list state)
  (let ((token1 (pop-preprocessor-directive-token directive-token-list directive-symbol)))
      (flet ((raise-unsyntactic-wcs-pragma-error ()
               (error 'preprocess-error
                      :format-control "Unsyntactic pragma '#pragma ~A ~A'"
                      :format-arguments (list +with-c-syntax-pragma+ token1))))
        (unless (symbolp token1)
          (raise-unsyntactic-wcs-pragma-error))
        (switch (token1 :test 'string=)
          (+end-of-inclusion-subdirective+
           (pop-include-state state))
          (otherwise
           (raise-unsyntactic-wcs-pragma-error))))))

(defun process-stdc-pragma (directive-symbol directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-stdc-pragma nil))
    (let* ((token1 (pop-preprocessor-directive-token directive-token-list directive-symbol))
           (token2 (pop-preprocessor-directive-token directive-token-list directive-symbol))
           on-off-switch)
      (check-no-preprocessor-token directive-token-list directive-symbol)
      (flet ((raise-unsyntactic-stdc-pragma-error ()
               (error 'preprocess-error
                      :format-control "Unsyntactic standard pragma '#pragma STDC ~A ~A'"
                      :format-arguments (list token1 token2)))
             (not-implemented-stdc-pragma-error ()
               (error 'preprocess-error
                      :format-contron "Current with-c-syntax does not implement '#pragma STDC ~A ~A'."
                      :format-arguments (list token1 token2))))
        (unless (and (symbolp token1) (symbolp token2))
          (raise-unsyntactic-stdc-pragma-error))
        (setf on-off-switch
              (switch (token2 :test 'string=)
                ("ON" t)
                ("OFF" nil)
                ("DEFAULT" :default)
                (otherwise (raise-unsyntactic-stdc-pragma-error))))
        (switch (token1 :test 'string=)
          ("FP_CONTRACT"
           (ecase on-off-switch
             ((:default) t)
             ((t nil)                   ; FIXME
              (not-implemented-stdc-pragma-error))))
          ("FENV_ACCESS"
           (ecase on-off-switch
             ((:default nil) t)
             ((t)                       ; FIXME
              (not-implemented-stdc-pragma-error))))
          ("CX_LIMITED_RANGE"
           (ecase on-off-switch
             ((:default nil) t)
             ((t)                       ; FIXME
              (not-implemented-stdc-pragma-error))))
          (otherwise
           (raise-unsyntactic-stdc-pragma-error)))))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|pragma|))
                                            directive-token-list state)
  (unless (preprocessor-token-exists-p directive-token-list)
    (warn 'with-c-syntax-style-warning
          :message "#pragma does not have any arguments.")
    (return-from process-preprocessing-directive nil))
  (let ((first-token
          (pop-preprocessor-directive-token directive-token-list directive-symbol)))
    (flet ((process-unknown-pragma ()
             (with-preprocessor-state-slots (state)
               (unless if-section-skip-reason
                 (warn 'with-c-syntax-style-warning
                       :message (format nil "'#pragma ~{~A~^ ~}' was ignored."
                                        (delete +whitespace-marker+ directive-token-list))))
               (return-from process-preprocessing-directive nil))))
      (unless (symbolp first-token)
        (process-unknown-pragma))
      (switch (first-token :test 'string=)
        (+with-c-syntax-pragma+
         (process-with-c-syntax-pragma directive-symbol directive-token-list state))
        ("STDC"
         (process-stdc-pragma directive-symbol directive-token-list state))
        ;; TODO: #pragma once
        (otherwise
         (process-unknown-pragma))))))

;;; Special preprocessor macro definitions

(define-constant +pp-date-month-name+
    #("(bad month)"
      "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  :test 'equalp
  :documentation "A vector of month names following asctime() manner. Used by __DATE__")

(defun with-c-syntax.preprocessor-special-macro:__DATE__ (&optional state)
  (declare (ignore state))
  (multiple-value-bind (_second _minute _hour date month year)
      (get-decoded-time)
    (declare (ignore _second _minute _hour))
    (format nil "~A ~2,' D ~4,'0D"
            (aref +pp-date-month-name+ month) date year)))

(defun with-c-syntax.preprocessor-special-macro:__FILE__ (state)
  (with-preprocessor-state-slots (state)
    (if file-pathname
        (namestring file-pathname))))

(defun with-c-syntax.preprocessor-special-macro:__LINE__ (state)
  (with-preprocessor-state-slots (state)
    line-number))

(defun with-c-syntax.preprocessor-special-macro:__TIME__ (&optional state)
  (declare (ignore state))
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

;;; Preprocessor loop helpers.

(defun preprocessor-loop-at-directive-p (state token)
  (with-preprocessor-state-slots (state)
    (and (symbolp token)
         (zerop tokens-in-line)
         (or (string= token "#")
             (and process-digraph? (string= token "%:"))))))

(defun preprocessor-loop-do-directives (state)
  "Process preprocessor directives. This is tranlation phase 4."
  (with-preprocessor-state-slots (state)
    (let ((token-list
            (draw-preprocessor-directive-line-tokens state)))
      (unless (preprocessor-token-exists-p token-list) ; Null directive
        (return-from preprocessor-loop-do-directives t))
      (let ((directive-token
              (pop-preprocessor-directive-token token-list "")))
        (flet ((raise-pp-error ()
                 (unless if-section-skip-reason
                   (error 'preprocess-error
                          :format-control "'~A' cannot be used as a preprocessing directive."
                          :format-arguments (list directive-token)))
                 (return-from preprocessor-loop-do-directives nil)))
          (unless (symbolp directive-token)
            (raise-pp-error))
          (if-let ((directive-symbol
                    (find-preprocessor-directive (symbol-name directive-token) readtable-case)))
            (process-preprocessing-directive directive-symbol token-list state)
            (raise-pp-error)))))))

(defun preprocessor-loop-try-intern-punctuators (state token)
  "Intern puctuators. this is with-c-syntax specific preprocessing path."
  (with-preprocessor-state-slots (state)
    (when-let (punctuator (find-punctuator (symbol-name token) process-digraph?))
      (push punctuator result-list)
      t)))

(defun preprocessor-loop-try-intern-keywords (state token)
  "Intern C keywords. this is with-c-syntax specific preprocessing path."
  (with-preprocessor-state-slots (state)
    (when-let (c-op (find-c-terminal (symbol-name token) readtable-case))
      (push c-op result-list)
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
         (when if-section-stack
           (error 'preprocess-error
                  :format-control "#if section does not end"))
         (when include-stack
           (error 'preprocess-error
                  :format-control "Internal error on processing #include. Please consult the author."))
         state)
      (cond
        ((eq token +whitespace-marker+)
         (progn))
        ((eq token +newline-marker+)
         (incf line-number)
         (setf tokens-in-line 0))
        ((preprocessor-loop-at-directive-p state token) ; Check directives before #if skips.
         (preprocessor-loop-do-directives state))
        (if-section-skip-reason         ; #if-section skips it.
         (progn))
        (t
         (typecase token
           (symbol
            (cond
              ;; Part of translation Phase 4 -- preprocessor macro
              ((preprocessor-macro-exists-p token)
               (let ((pp-macro (find-preprocessor-macro token)))
                 (cond ((and (symbolp pp-macro) ; Special macros
                             (eql (symbol-package pp-macro)
                                  (find-package '#:with-c-syntax.preprocessor-special-macro)))
                        (push (funcall pp-macro state) result-list))
                       ((functionp pp-macro) ; preprocessor funcion
	                (multiple-value-bind (macro-arg new-lis)
		            (collect-preprocessor-macro-arguments token-list)
		          (push (apply pp-macro macro-arg) result-list)
		          (setf token-list new-lis)))
	               (t               ; symbol expansion
                        (setf token-list
                              (append pp-macro token-list))))))
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
                       nil))))          ; fallthrough
              (t
               (push token result-list))))
           (string
            (preprocessor-loop-concatenate-string state token))
           (t
            (push token result-list)))
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
           (make-instance 'preprocessor-state
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
