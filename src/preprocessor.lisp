(in-package #:with-c-syntax.core)

(defvar *with-c-syntax-preprocessor-process-digraph* nil
  "Determines whether preprocessor replaces digraphs.
 If this is true, replacement occurs but `with-c-syntax-style-warning' is signalled.
 If this is `:no-warn', replacement occurs and the style-warning is not signalled.")

(defun find-asdf-system-relative-file (base name)
  "See `find-with-c-syntax-default-include-file'"
  (let* ((name-list (if (listp name)
                        name
                        (split-sequence:split-sequence #\/ (string name))))
         (component (or (asdf:find-component base name-list)
                        (asdf:find-component base (mapcar 'string-downcase name-list)))))
    (if component
        (asdf:component-pathname component))))

(defun find-with-c-syntax-default-include-file (name)
  "Makes a pathname of the directory containing with-c-syntax include
 files. This function is used for making the default value of
 `*with-c-syntax-find-include-file-function-list*'"
  (find-asdf-system-relative-file '(:with-c-syntax . "include") name))

(defvar *with-c-syntax-find-include-file-function-list*
  (list 'find-with-c-syntax-default-include-file)
  "List of functions used for searching include files with '#include <...>' style.
 Each function should take one argument and returns a pathname, or nil if failed.
 See `find-include-<header>-file'.")

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

;;; Preprocessor macro expansion

(defun va-args-identifier-p (token)
  (token-equal-p token :__VA_ARGS__))

(defclass macro-definition ()
  ((name :initarg :name :reader macro-definition-name)
   (replacement-list :initarg :replacement-list
                     :reader macro-definition-replacement-list)))

(defmethod cl:initialize-instance :before ((macro-definition macro-definition)
                                           &rest args &key name)
  (declare (ignorable args))
  (when (va-args-identifier-p name)
    (error 'preprocess-error
	   :format-control "Macro name '__VA_ARGS__' is now allowed as an user defined macro.")))

(defmethod cl:initialize-instance :after ((macro-definition macro-definition) &rest args)
  (declare (ignorable args))
  (with-slots (replacement-list) macro-definition
    (when (eq (first replacement-list) +whitespace-marker+)
      (pop replacement-list))
    (when (eq (lastcar replacement-list) +whitespace-marker+)
      (setf replacement-list (butlast replacement-list)))))

(defgeneric check-valid-macro-redefinition (macro-definition1 macro-definition2 name)
  (:method (mdef1 mdef2 name)
    (error 'preprocess-error
           :format-control "Macro '~A' redefinition from ~A type to ~A type is invalid."
           :format-arguments (list name (type-of mdef1) (type-of mdef2)))))

(defgeneric check-replacement-list-equal (mdef1 mdef2 name)
  (:method ((mdef1 macro-definition) (mdef2 macro-definition) name)
    (let ((r-list-1 (macro-definition-replacement-list mdef1))
          (r-list-2 (macro-definition-replacement-list mdef2)))
      (unless (replacement-list-equal r-list-1 r-list-2)
        (error 'preprocess-error
               :format-control "Macro '~A' redefinition is invalid because replacement-lists are different.~%Old: ~A~%New: ~A"
               :format-arguments (list name r-list-1 r-list-2))))))

(defclass object-like-macro (macro-definition)
  ())

(defmethod cl:print-object ((macro-definition object-like-macro) stream)
  (print-unreadable-object (macro-definition stream :type t :identity t)
    (princ (macro-definition-name macro-definition) stream)))

(defmethod cl:initialize-instance :before ((macro-definition object-like-macro)
                                           &rest args &key replacement-list)
  (declare (ignorable args))
  (when (some #'va-args-identifier-p replacement-list)
    (error 'preprocess-error
	   :format-control "Object-like-macro cannot have '__VA_ARGS__' in its replacement-list.")))

(defun replacement-list-equal (list1 list2)
  (tree-equal list1 list2
              :test
              (lambda (x y)
                (if (and (preprocessing-number-p x) (preprocessing-number-p y))
                    (equal (preprocessing-number-string x) (preprocessing-number-string y))
                    (equal x y)))))

(defmethod check-valid-macro-redefinition ((mdef1 object-like-macro) (mdef2 object-like-macro) name)
  (check-replacement-list-equal mdef1 mdef2 name))

(defclass function-like-macro (macro-definition)
  ((identifier-list :initarg :identifier-list
                    :reader function-like-macro-identifier-list)
   (variadicp :initarg :variadicp
              :reader function-like-macro-variadicp)))

(defmethod cl:print-object ((macro-definition function-like-macro) stream)
  (print-unreadable-object (macro-definition stream :type t :identity t)
    (format stream "~A~:A"
            (macro-definition-name macro-definition)
            (function-like-macro-identifier-list macro-definition))))

(defmethod cl:initialize-instance :before ((macro-definition function-like-macro)
                                           &rest args &key identifier-list variadicp replacement-list)
  (declare (ignorable args))
  (when (some #'va-args-identifier-p identifier-list)
    (error 'preprocess-error
           :format-control "Function-like-macro cannot have '__VA_ARGS__' in its identifier-list."))
  (unless (length= identifier-list (remove-duplicates identifier-list))
    (error 'preprocess-error
           :format-control "Function-like-macro has duplicated parameter names."))
  (when (and (not variadicp)
             (some #'va-args-identifier-p replacement-list))
    (error 'preprocess-error
           :format-control "Non-variadic Function-like-macro cannot have '__VA_ARGS__' in its replacement-list.")))

(defmethod check-valid-macro-redefinition ((mdef1 function-like-macro) (mdef2 function-like-macro) name)
  (check-replacement-list-equal mdef1 mdef2 name)
  (let ((i-list-1 (function-like-macro-identifier-list mdef1))
        (i-list-2 (function-like-macro-identifier-list mdef2)))
    (unless (equal i-list-1 i-list-2)
      (error 'preprocess-error
             :format-control "Macro '~A' redefinition is invalid because identifier-lists are different.~%Old: ~A~%New: ~A"
             :format-arguments (list name i-list-1 i-list-2))))
  (let ((variadicp-1 (function-like-macro-variadicp mdef1))
        (variadicp-2 (function-like-macro-variadicp mdef2)))
    (unless (equal variadicp-1 variadicp-2)
      (error 'preprocess-error
             :format-control "Macro '~A' redefinition is invalid because variance are different.~%Old: ~A~%New: ~A"
             :format-arguments (list name variadicp-1 variadicp-2)))))

(defun preprocessor-macro-exists-p (macro-alist symbol)
  (or (find-symbol (symbol-name symbol) '#:with-c-syntax.predefined-macro)
      (when-let ((entry (assoc symbol macro-alist)))
        (not (eql (cdr entry) :macro-suppressed)))))

(defun whitespace-like-token-p (token)
  (or (eq token +whitespace-marker+)
      (eq token +newline-marker+)))

(defun delete-consecutive-whitespace-marker (token-list)
  (loop for cons = token-list then next
        for next = (cond
                     ((and (whitespace-like-token-p (first cons))
                           (whitespace-like-token-p (second cons)))
                      (setf (cdr cons) (cddr cons))
                      cons)
                     (t
                      (cdr cons)))
        while cons)
  token-list)

(define-condition incompleted-macro-arguments-error (preprocess-error)
  ((token-list :initarg :token-list))
  (:report
   (lambda (condition stream)
     (format stream "preprocessor macro argument is incompleted. tokens: ~S"
             (slot-value condition 'token-list)))))

(defstruct pp-macro-argument
  (token-list)       ; Still holds `:end-of-preprocessor-macro-scope'.
  (token-list-expansion nil)
  (macro-alist))      ; The context for macro expansion.

(defun macro-scoping-marker-p (token)
  (or (eql token :end-of-preprocessor-macro-scope)
      (typep token 'macro-definition)))

(defun macro-control-marker-p (token)
  (or (whitespace-like-token-p token)
      (macro-scoping-marker-p token)))

(defun collect-one-macro-argument (token-list macro-alist)
  (loop
    with macro-alist-result = macro-alist
    with nest-level of-type fixnum = 0
    for token-cons on token-list
    as token = (car token-cons)
    if (typep token 'macro-definition)
      do (push (cons (macro-definition-name token) :macro-suppressed)
               macro-alist-result)
    else if (eq token :end-of-preprocessor-macro-scope)
           do (pop macro-alist-result)
    else
      do (flet ((return-this-arg ()
                  (return-from collect-one-macro-argument
                    (values (make-pp-macro-argument :token-list arg-tokens
                                                    :macro-alist macro-alist)
                            (cdr token-cons)
                            macro-alist-result
                            token))))
           (switch (token :test 'token-equal-p)
             ("("
              (incf nest-level))
             (","
              (when (zerop nest-level)
                (return-this-arg)))
             (")"
              (cond ((zerop nest-level)
                     (return-this-arg))
                    (t
                     (decf nest-level))))))
      and collect token into arg-tokens
    finally
       (error 'incompleted-macro-arguments-error :token-list token-list)))

(defun collect-preprocessor-macro-arguments (token-list macro-alist)
  (let ((args-start
          (loop for token-cons on token-list
                as token = (car token-cons)
                if (whitespace-like-token-p token) 
                  do (progn)
                else if (typep token 'macro-definition)
                       do (push (cons (macro-definition-name token) :macro-suppressed)
                                macro-alist)
                else if (eql token :end-of-preprocessor-macro-scope)
                       do (pop macro-alist)
                else
                  do (assert (token-equal-p token "(") ()
                             'preprocess-error
                             :format-control "This is a BUG of function-like-macro-invocation-p.")
                  and return (cdr token-cons))))
    (loop
      with token-cons = args-start
      while token-cons
      for (pp-macro-arg rest-token-list new-macro-alist separator-token)
        = (multiple-value-list
           (collect-one-macro-argument token-cons macro-alist))
      collect pp-macro-arg into macro-arg-results
      do (setf token-cons rest-token-list
               macro-alist new-macro-alist)
         (switch (separator-token :test 'token-equal-p)
           (","
            (progn))
           (")"
            (return (values macro-arg-results token-cons macro-alist))))
      finally
         (error 'incompleted-macro-arguments-error :token-list token-list))))

(defun expand-each-preprocessor-macro-in-list (token-list macro-alist pp-state)
  "Used by `expand-macro-argument', #line and #include."
  (loop for token-cons on token-list ; Do not use `pop-preprocessor-directive-token' for preserving `+whitespace-marker+'.
        as token = (first token-cons)
        if (and (symbolp token)
                (preprocessor-macro-exists-p macro-alist token))
          append (multiple-value-bind (expansion rest-tokens new-macro-alist)
                     (expand-preprocessor-macro token (rest token-cons) macro-alist pp-state)
                   (setf token-cons (list* :loop-on-clause-dummy rest-tokens)
                         macro-alist new-macro-alist)
                   expansion)
        else if (typep token 'macro-definition)
               do (push (cons (macro-definition-name token) :macro-suppressed)
                        macro-alist)
        else if (eql token :end-of-preprocessor-macro-scope)
               do (pop macro-alist)
        else
          collect token))

(defun expand-macro-argument (pp-macro-argument pp-state)
  (let ((token-list (pp-macro-argument-token-list pp-macro-argument))
        (macro-alist (pp-macro-argument-macro-alist pp-macro-argument)))
    (let ((all-expansions
            (expand-each-preprocessor-macro-in-list token-list macro-alist pp-state)))
      (setf all-expansions (delete-consecutive-whitespace-marker all-expansions)
            (pp-macro-argument-token-list-expansion pp-macro-argument) all-expansions)
      all-expansions)))

(defun make-macro-arguments-alist (macro-definition macro-arg-list)
  "Makes an alist of (<symbol> . <macro-argument>) for function-like-macro expansion."
  (loop
    with macro-identifier-list = (function-like-macro-identifier-list macro-definition)
    for marg-cons on macro-arg-list
    and identifier-cons on macro-identifier-list
    collect (cons (first identifier-cons) (first marg-cons))
      into result-alist
    finally
       (cond
         ((assert (not (and marg-cons identifier-cons))))
         ((and (endp marg-cons) identifier-cons)
          (error 'preprocess-error
                 :format-control "Insufficient arguments for macro '~A'"
                 :format-arguments (list (macro-definition-name macro-definition))))
         ((and marg-cons (endp identifier-cons))
          (cond
            ((and (length= 0 macro-identifier-list)
                  (length= 1 macro-arg-list)
                  (length= 0 (pp-macro-argument-token-list (first macro-arg-list)))
                  (not (function-like-macro-variadicp macro-definition)))
             ;; Macro invocation like 'X()' may mean one of followings:
             ;; - Passing no arguments to X.
             ;; - Passing one argument consists of empty tokens.
             ;; This check looks where the first situation or not.
             ;; 
             ;; See 'q()' macro usage of the Example3, in ISO/IEC 9899:1999
             ;; page 156, '6.1.3.5 Scope of macro definitions'.
             (push (cons (first macro-identifier-list)
                         (make-pp-macro-argument :token-list nil
                                                 :token-list-expansion nil
                                                 :macro-alist nil))
                   result-alist))
            ((not (function-like-macro-variadicp macro-definition))
             (error 'preprocess-error
                    :format-control "Too many arguments for macro '~A'"
                    :format-arguments (list (macro-definition-name macro-definition))))
            (t
             (loop with comma-token = 'with-c-syntax.punctuator:|,|
                   for at-first = t then nil
                   for marg in marg-cons
                   as last-macro-alist = (pp-macro-argument-macro-alist marg)
                   unless at-first      ; Insert comma between each args.
                     collect comma-token into tokens
                     and collect comma-token into expansions
                   append (pp-macro-argument-token-list marg) into tokens
                   append (pp-macro-argument-token-list-expansion marg) into expansions
                   finally
                      (push (cons (intern (string :__VA_ARGS__)) 
                                  (make-pp-macro-argument :token-list tokens
                                                          :token-list-expansion expansions
                                                          :macro-alist last-macro-alist))
                            result-alist)))))
         (t            ; (and (endp marg-cons) (endp identifier-cons))
          (when (function-like-macro-variadicp macro-definition)
            (warn 'with-c-syntax-style-warning
                  :message "Variadic macro does not have any arguments. __VA_ARGS__ bound to nil.")
            (push (cons (intern (string :__VA_ARGS__)) 
                        (make-pp-macro-argument :token-list nil
                                                :token-list-expansion nil
                                                :macro-alist nil))
                  result-alist))))
       (return result-alist)))

(defgeneric stringification-separator-required-p (prev-token)
  (:method ((prev-token (eql +whitespace-marker+)))
    nil)
  (:method ((prev-token (eql +newline-marker+)))
    nil)
  (:method ((prev-token string))
    nil)
  (:method ((prev-token character))
    nil)
  (:method ((prev-token preprocessing-number))
    t)
  (:method ((prev-token symbol))
    (not (find-punctuator (symbol-name prev-token) t))))

(defun expand-stringify-operator (token macro-arg-alist)
  (unless (symbolp token)
    (error 'preprocess-error
           :format-control "# operator is used to bad argument: '~A'"
           :format-arguments (list token)))
  (unless (assoc token macro-arg-alist)
    (error 'preprocess-error
           :format-control "# operator argument '~A' is not a macro parameter."
           :format-arguments (list token)))
  (with-output-to-string (out)
    (loop with marg = (cdr (assoc token macro-arg-alist))
          for prev = +whitespace-marker+ then i
          for i in (pp-macro-argument-token-list marg)
          do (cond
               ((macro-scoping-marker-p i)
                (setf i prev))         ; Keeps prev.
               ((whitespace-like-token-p i)
                (unless (whitespace-like-token-p prev)
                  (write-char #\space out)))
               ((preprocessing-number-p i)
                (when (stringification-separator-required-p prev)
                  (write-char #\space out))
                (princ (preprocessing-number-string i) out))
               ((characterp i)
                (format out "'~C'" i))
               ((stringp i)
                (princ "\"" out)
                (loop for c across i
                      if (or (char= c #\") (char= c #\\))
                        do (write-char #\\ out) ; escape double-quote and backslash
                      do (write-char c out))
                (princ "\"" out))
               ((symbolp i)
                (assert (not (macro-control-marker-p i)))
                (when (and (not (find-punctuator (symbol-name i) t))
                           (stringification-separator-required-p prev))
                  (write-char #\space out))
                (princ i out))
               (t
                (assert (not (macro-control-marker-p i)))
                (princ i out))))))

(defconstant +placemaker-token+ '||)

(defgeneric concatenate-token (token1 token2)
  (:documentation "Perform token concatenation caused by '##' operator.")
  (:method (token1 token2)
    (error 'preprocess-error
           :format-control "Token concatenation '~A' and '~A' is not supported."
           :format-arguments (list token1 token2))))

(defmethod concatenate-token ((token1 (eql +placemaker-token+)) token2)
  token2)

(defmethod concatenate-token (token1 (token2 (eql +placemaker-token+)))
  token1)

(defmethod concatenate-token ((token1 symbol) (token2 symbol))
  (cond
    ((eq token1 +placemaker-token+) token2)
    ((eq token2 +placemaker-token+) token1)
    (t
     (unless (eql (symbol-package token1) (symbol-package token2))
       (warn 'with-c-syntax-style-warning
             :message (format nil "## operand '~A' and '~A' belong different packages. The package of '~A' is used."
                              token1 token2 token1)))
     (intern (concatenate 'string (symbol-name token1) (symbol-name token2))))))

(defun stringify-conced-integer (int)
  (with-standard-io-syntax
    (princ-to-string int)))

(defmethod concatenate-token ((token1 symbol) (token2 integer))
  (intern (concatenate 'string (symbol-name token1)
                       (stringify-conced-integer token2))))

(defmethod concatenate-token ((token1 integer) (token2 symbol))
  (intern (concatenate 'string
                       (stringify-conced-integer token1)
                       (symbol-name token2))))

(defun intern-conc-pp-number (str1 str2 &optional (package *package*))
  (let* ((conc (concatenate 'string str1 str2)))
    (handler-case
        (read-preprocessing-number-from-string conc)
      (with-c-syntax-reader-error ()
        (intern conc package)))))

(defmethod concatenate-token ((token1 integer) (token2 integer))
  ;; Firstly I thought this can implement with numeric operatons, but
  ;; how to treat '10 ## -10' ?
  (intern-conc-pp-number (stringify-conced-integer token1)
                         (stringify-conced-integer token2)))

(defmethod concatenate-token ((token1 preprocessing-number) (token2 preprocessing-number))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (preprocessing-number-string token2)))

(defmethod concatenate-token ((token1 preprocessing-number) (token2 symbol))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (symbol-name token2)))

(defmethod concatenate-token ((token1 symbol) (token2 preprocessing-number))
  (intern-conc-pp-number (symbol-name token1)
                         (preprocessing-number-string token2)))
                         
(defmethod concatenate-token ((token1 preprocessing-number) (token2 integer))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (stringify-conced-integer token2)))

(defmethod concatenate-token ((token1 integer) (token2 preprocessing-number))
  (intern-conc-pp-number (stringify-conced-integer token1)
                         (preprocessing-number-string token2)))

(defun split-list-at (list pos)
  (loop with target = '#0=#:unspecific
        for count from 0
        for i in list
        if (< count pos)
          collect i into before
        else if (= count pos)
               do (setf target i)
        else
          collect i into after
        finally
           (when (eq target '#0#)
             (error "split-list-at POS '~D' is out-of-range of LIST ~A."
                    pos list))
           (return (values before target after))))

(defun split-concatenate-operator-left-operand (token-list)
  (let ((last-token-pos
          (position-if-not #'macro-control-marker-p token-list :from-end t)))
    (if last-token-pos
        (multiple-value-bind (before last-token after)
            (split-list-at token-list last-token-pos)
          (values before
                  last-token
                  (delete-whitespace-marker after)))
        (values (copy-list token-list)
                +placemaker-token+
                nil))))

(defun split-concatenate-operator-right-operand (token-list)
  (let ((first-token-pos
          (position-if-not #'macro-control-marker-p token-list)))
    (if first-token-pos
        (multiple-value-bind (before first-token after)
            (split-list-at token-list first-token-pos)
          (values (delete-whitespace-marker before)
                  first-token
                  after))
        (values nil
                +placemaker-token+
                (copy-list token-list)))))

(defun expand-concatenate-operator (token1-list token2-list)
  (multiple-value-bind (t1-before t1-token t1-after)
      (split-concatenate-operator-left-operand token1-list)
    (multiple-value-bind (t2-before t2-token t2-after)
        (split-concatenate-operator-right-operand token2-list)
      (values
       (nconc t1-before t1-after)
       (concatenate-token t1-token t2-token)
       (nconc t2-before t2-after)))))

(defun expand-macro-replacement-list (replacement-list macro-arg-alist process-digraph?)
  (loop
    with rev-expansion = nil
    for token-cons on replacement-list
    as token = (first token-cons)
    as next-token-cons = (if (whitespace-like-token-p (second token-cons))
                             (cddr token-cons)
                             (cdr token-cons))
    as macro-arg-alist-cons = (assoc token macro-arg-alist)
    
    if (or (token-equal-p token "##") ; Concatenation.
           (and process-digraph? (token-equal-p token "%:%:")))
      do (let* ((token1-list
                  (loop
                    initially
                       (when (null rev-expansion)
                         (error 'preprocess-error
                                :format-control "The first operand of '##' does not exists."))
                    with ret = nil
                    for i = (pop rev-expansion)
                    while i
                    do (push i ret)
                    while (macro-control-marker-p i)
                    finally (return ret)))
                (token2-list
                  (if (endp next-token-cons)
                      (error 'preprocess-error
                             :format-control "The second operand of '##' does not exists.")
                      (let ((token2 (car next-token-cons)))
                        (if-let ((entry (assoc token2 macro-arg-alist)))
                          (pp-macro-argument-token-list (cdr entry))
                          (list token2))))))
           (multiple-value-bind (before-token-list conc-result after-token-list)
               (expand-concatenate-operator token1-list token2-list)
             (setf token-cons
                   (list* :loop-on-clause-dummy
                          (nconc after-token-list
                                 (rest next-token-cons)))) ; removes token2
             (nreconcf rev-expansion before-token-list)
             (push conc-result rev-expansion)))
    else if (or (token-equal-p token "#") ; Stringify.
                (and process-digraph? (token-equal-p token "%:")))
           do (cond
                ((endp next-token-cons)
                 (error 'preprocess-error
                        :format-control "The second operand of '#' does not exists."))
                ((token-equal-p (car next-token-cons) "##")
                 ;; Special case. See `with-c-syntax.test::test-pp-6.10.3.3-example'.
                 (push token rev-expansion))
                (t
                 (setf token-cons (list* :loop-on-clause-dummy (cdr next-token-cons)))
                 (push (expand-stringify-operator (car next-token-cons) macro-arg-alist)
                       rev-expansion)))
    else if macro-arg-alist-cons        ; Replacement
           do (revappendf rev-expansion
                          (pp-macro-argument-token-list-expansion (cdr macro-arg-alist-cons)))
    else
      do (push token rev-expansion)
    finally
       (return (delete-consecutive-whitespace-marker
                (delete +placemaker-token+ ; Placemaker-token should not be left after macro expansion. 
                        (nreverse rev-expansion))))))

(defun expand-function-like-macro (macro-definition rest-token-list macro-alist pp-state)
  (multiple-value-bind (macro-arg-list tail-of-rest-token-list new-macro-alist)
      (collect-preprocessor-macro-arguments rest-token-list macro-alist)
    (dolist (marg macro-arg-list)
      (expand-macro-argument marg pp-state))
    (let ((macro-arg-alist
            (make-macro-arguments-alist macro-definition macro-arg-list)))
      (values 
       (expand-macro-replacement-list (macro-definition-replacement-list macro-definition)
                                      macro-arg-alist
                                      (pp-state-process-digraph? pp-state))
       tail-of-rest-token-list
       new-macro-alist))))

(defun function-like-macro-invocation-p (token-list)
  (let ((first-token
          (find-if-not #'macro-control-marker-p token-list)))
    (and first-token
         (token-equal-p first-token "("))))

(defun expand-object-like-macro (macro-definition pp-state)
  (expand-macro-replacement-list (macro-definition-replacement-list macro-definition)
                                 nil
                                 (pp-state-process-digraph? pp-state)))

(defun expand-predefined-macro (predefined-macro pp-state)
  (cond ((boundp predefined-macro)
         (symbol-value predefined-macro))
        ((fboundp predefined-macro)
         (funcall predefined-macro pp-state))
        (t
         (error 'preprocess-error
                :format-control "Predefined macro ~A is not bound nor fbound."
                :format-arguments (list predefined-macro)))))

(defun expand-preprocessor-macro (token rest-token-list macro-alist pp-state)
  "Expand preprocessor macro named TOKEN. If it is a function-like
macro, its arguments are taken from REST-TOKEN-LIST. MACRO-ALIST is an
alist of preprocessor macro definitions.  PP-STATE is a
`preprocessor-state' object, used for expanding predefined macros.

 Returns four values:
 1. A list of tokens made by expansion.
 2. A list, tail of REST-TOKEN-LIST, left after collecting function-like macro arguments.
 3. An alist derived from MACRO-ALIST changed after expansion.
 4. A boolean tells whether TOKEN is a function-like-macro but
    REST-TOKEN-LIST is not a form of invocation."
  (when-let ((predefined-macro
              (find-symbol (symbol-name token) '#:with-c-syntax.predefined-macro)))
    (let ((expansion (expand-predefined-macro predefined-macro pp-state)))
      (return-from expand-preprocessor-macro
        (values (list expansion)
                rest-token-list
                macro-alist))))
  (let* ((pp-macro-entry (assoc token macro-alist))
         (pp-macro (cdr pp-macro-entry)))
    (etypecase pp-macro
      (function-like-macro
       (if (function-like-macro-invocation-p rest-token-list)
           (multiple-value-bind (expansion tail-token-list new-macro-alist)
               (expand-function-like-macro pp-macro rest-token-list macro-alist pp-state)
             (values (nconc (list pp-macro)
                            expansion
                            (list :end-of-preprocessor-macro-scope))
                     tail-token-list
                     new-macro-alist))
           (values (list token)
                   rest-token-list
                   macro-alist
                   t)))
      (object-like-macro
       (let ((expansion (expand-object-like-macro pp-macro pp-state)))
         (values (nconc (list pp-macro)
                        expansion
                        (list :end-of-preprocessor-macro-scope))
                 rest-token-list
                 macro-alist))))))


;;; Identifier split.

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
		      (intern suffix))))))

(defun find-operator-on-suffix (operator symbol)
  "This function tries to find OPERATOR in back of SYMBOL.
If found, it returns T and list of of splited symbols.  If not found,
it returns NIL."
  (let ((sym-name (symbol-name symbol))
	(op-name (symbol-name operator)))
    (if (ends-with-subseq op-name sym-name)
	(values t
                (list (intern (subseq sym-name 0 (- (length sym-name) (length op-name))))
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
  ((process-digraph? :initarg :process-digraph? :initform nil
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
   (macro-alist :initform () :type list
                :accessor pp-state-macro-alist)
   (if-section-stack :initform nil :type list
                     :accessor pp-state-if-section-stack)
   (if-section-skip-reason :initform nil
                           :accessor pp-state-if-section-skip-reason)
   (include-stack :initform nil
                  :accessor pp-state-include-stack)))

;; TODO: reduce its usage
(defmacro with-preprocessor-state-slots ((state) &body body)
  `(with-accessors ((process-digraph? pp-state-process-digraph?)
                    (file-pathname pp-state-file-pathname)
                    (token-list pp-state-token-list)
                    (result-list pp-state-result-list)
                    (line-number pp-state-line-number)
                    (tokens-in-line pp-state-tokens-in-line)
                    (macro-alist pp-state-macro-alist)
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

(defun preprocessor-token-exists-p (token-list)
  (loop for token in token-list
        thereis (not (whitespace-like-token-p token))))

(defun token-equal-p (token name)
  (and (symbolp token)
       (string= token name)))

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

;;; #if sections.

(defclass if-section ()
  ((group-conditions :initform (make-array 1 :fill-pointer 0 :adjustable t)
                      :type vector)))

(defmethod cl:print-object ((obj if-section) stream)
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

(defun expand-defined-operator (token-list macro-alist process-digraph? directive-symbol)
  (loop while (preprocessor-token-exists-p token-list)  ; FIXME: this is too slow..
        collect
        (let ((token (pop-preprocessor-directive-token token-list directive-symbol :errorp nil)))
          (cond
            ((pp-defined-operator-p token)
             (let* ((next-token
                      (pop-preprocessor-directive-token token-list directive-symbol))
                    (param
                      (if (token-equal-p next-token "(")
                          (prog1 (pop-preprocessor-directive-token token-list directive-symbol)
                            (let ((r-paren?
                                    (pop-preprocessor-directive-token token-list directive-symbol)))
                              (unless (token-equal-p r-paren? ")")
                                (error
                                 'preprocess-error
	                         :format-control "'defined' operator does not have corresponding ')'. '~A' was found."
	                         :format-arguments (list r-paren?)))))
                          next-token)))
               (when (or (not (symbolp param))
                         (find-punctuator (symbol-name param) process-digraph?))
                 (error
                  'preprocess-error
	          :format-control "'defined' operator takes only identifiers. '~A' was passed."
	          :format-arguments (list param)))
               (if (preprocessor-macro-exists-p macro-alist param) 1 nil)))
            (t
             token)))))

(defun eval-if-expression (directive-symbol directive-token-list state)
  (with-preprocessor-state-slots (state)
    (let* ((expansion
             (expand-defined-operator directive-token-list macro-alist
                                      process-digraph? directive-symbol))
           (expansion
             (expand-each-preprocessor-macro-in-list expansion macro-alist state))
           (expansion
             (remove-if #'macro-scoping-marker-p expansion)) ; TODO: move to the next lexer?
           (lexer
             (pp-if-expression-lexer expansion process-digraph?))
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
      result)))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|if|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (begin-if-section state '(:skipped :if) nil)
      (return-from process-preprocessing-directive nil))
    (unless (preprocessor-token-exists-p directive-token-list)
      (raise-no-preprocessor-token-error directive-symbol))
    (let ((result (eval-if-expression directive-symbol directive-token-list state)))
      (begin-if-section state directive-token-list result))))

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
           (result (preprocessor-macro-exists-p macro-alist identifier)))
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
           (result (not (preprocessor-macro-exists-p macro-alist identifier))))
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
         (unless (preprocessor-token-exists-p directive-token-list)
           (raise-no-preprocessor-token-error directive-symbol))
         (let ((result (eval-if-expression directive-symbol directive-token-list state)))
           (if-section-add-group current-if-section directive-token-list result)
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

;;; #include

(defun parse-header-name (token-list directive-symbol try-pp-macro-expand macro-alist pp-state)
  ;; FIXME: Current header-name implementation does not recognize implementation-defined points.
  (let ((token1 (pop-preprocessor-directive-token token-list directive-symbol)))
    (cond
      ((stringp token1)
       ;; FIXME: Treating of these chars is implementation-defined: ', \, //, or /* .
       ;; See ISO/IEC 9899:1999, page 64.
       (check-no-preprocessor-token token-list directive-symbol)
       (values token1 :q-char-sequence))
      ((and (symbolp token1)
            (starts-with #\< (symbol-name token1)))
       (let ((name (symbol-name token1)))
         (cond
           ((length= 1 name) ; For reader Level 2 -- Splited like < iso646 . h >
            ;; FIXME: Treating of these chars is implementation-defined: ', \, ", //, or /* .
            ;;        See ISO/IEC 9899:1999, page 64.
            ;; FIXME: The number of whitespaces is not preserved. To fix it,
            ;;        I must treat whitespaces specially only after '#include' by our reader.
            (loop for token-cons on token-list
                  as token = (first token-cons)
                  when (token-equal-p token ">")
                    do (pop token-cons) (loop-finish)
                  collect
                  (cond ((eq token +whitespace-marker+)
                         #\space)
                        ((symbolp token)
                         (symbol-name token))
                        (t
                         token))
                    into h-tokens
                  finally
                     (check-no-preprocessor-token token-cons directive-symbol)
                     (return (values (format nil "~{~A~}" h-tokens)
                                     :h-char-sequence))))
           ((ends-with #\> name) ; For reader Level 1 -- Like '<iso646.h>'
            (check-no-preprocessor-token token-list directive-symbol)
            ;; I thought I may use `string-trim', but what to do when I saw '<<stdio.h>' ?
            (values (subseq name 1 (1- (length name)))
                    :h-char-sequence))
           (t
            ;; Like '#include <hoge.h >' in level1 syntax.
            (error 'preprocess-error
              :format-control "'~A' does not have '>'"
              :format-arguments (list token1 directive-symbol))))))
      (try-pp-macro-expand
       (push token1 token-list)
       (let ((expanded (expand-each-preprocessor-macro-in-list token-list macro-alist pp-state)))
         (return-from parse-header-name
           (parse-header-name expanded directive-symbol nil nil nil))))
      (t
       (error 'preprocess-error
              :format-control "'~A' cannot be used for #~A"
              :format-arguments (list token1 directive-symbol))))))

(defclass saved-include-state ()
  ((if-section-stack :initarg :if-section-stack
                     :reader saved-include-state-if-section-stack)
   (package :initarg :package :type package
            :reader saved-include-state-package)
   (readtable :initarg :readtable :type readtable
              :reader saved-include-state-readtable)))

(defmethod push-include-state ((state preprocessor-state))
  (with-preprocessor-state-slots (state)
    (let ((i-state (make-instance 'saved-include-state
                                  :if-section-stack if-section-stack
                                  :package *package*
                                  :readtable *readtable*)))
      (push i-state include-stack))))

(defmethod pop-include-state ((state preprocessor-state))
  (with-preprocessor-state-slots (state)
    (let ((i-state (pop include-stack)))
      (unless (eql if-section-stack
                   (saved-include-state-if-section-stack i-state))
        (error 'preprocess-error
               :format-control "#if section does not end in included file ~A"
               :format-arguments (list file-pathname)))
      (setf *package* (saved-include-state-package i-state)
            *readtable* (saved-include-state-readtable i-state) )
      i-state)))

(defun find-include-<header>-file (header-name &key (errorp t))
  "Finds a file specified by #include <...> style header-name with
 pathnames got by `*with-c-syntax-find-include-file-function-list*'."
  (loop for func in *with-c-syntax-find-include-file-function-list*
        as path = (funcall func header-name)
        if path
          return it
        finally
           (when errorp
             (error 'preprocess-include-file-error
                    :pathname header-name))))

(defun find-include-header-file (header-name &key (errorp t))
  "Finds a file specified by #include \"...\" style header-name.
 Current strategy is just to use `cl:probe-file'. So it will affected
 by `*default-pathname-defaults*'.
 If no file was found, `find-include-<header>-file' is called."
  (or (probe-file header-name)
      (find-include-<header>-file header-name :errorp errorp)))

(defun tokenize-included-source-file (header-name state)
  (let* ((line-sym (ecase (readtable-case *readtable*)
                     ((:upcase :invert) '|LINE|)
                     ((:downcase :preserve) '|line|)))
         (start-line-tokens
           (list '|#| line-sym 1 header-name +newline-marker+))
         (main-tokens
           ;; Included file is read at same reader level and same readtable-case.
           (with-open-file (stream header-name)
             (tokenize-source stream nil)))
         (end-tokens
           (list
            ;; I once used #pragma for marking the end of inclusion,
            ;; but I changed it to a special token because #pragma
            ;; should be ignored by #if sections.
            :end-of-inclusion +newline-marker+))
         (end-line-tokens
           (if (pp-state-file-pathname state)
               (list '|#| line-sym (pp-state-line-number state) (pp-state-file-pathname state) +newline-marker+)
               (list '|#| line-sym (pp-state-line-number state) +newline-marker+))))
    (nconc start-line-tokens
           main-tokens
           end-tokens
           end-line-tokens)))

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
        (parse-header-name directive-token-list directive-symbol t macro-alist state)
      (let* ((pathname (ecase header-type
                         (:q-char-sequence (find-include-header-file header-name))
                         (:h-char-sequence (find-include-<header>-file header-name))))
             (included-tokens
               (tokenize-included-source-file pathname state)))
        (push-include-state state)
        (pop-next-newline token-list)
        (setf token-list
              (nconc included-tokens token-list))))))

;;; #define

(defun add-local-preprocessor-macro (state symbol macro-definition)
  (with-accessors ((macro-alist pp-state-macro-alist)) state
    (if-let ((old-def-entry (assoc symbol macro-alist)))
      (progn
        (check-valid-macro-redefinition (cdr old-def-entry) macro-definition symbol)
        (setf (cdr old-def-entry) macro-definition))
      (push (cons symbol macro-definition) macro-alist))))

(defun collect-function-like-macro-identifier-list
    (token-list &optional (directive-symbol 'with-c-syntax.preprocessor-directive:|define|))
  (loop
    with variadicp = nil
      initially (assert (token-equal-p
                         '|(|
                         (pop-preprocessor-directive-token token-list directive-symbol))) 
    for i = (pop-preprocessor-directive-token token-list directive-symbol)
    unless (symbolp i)
      do (error 'preprocess-error
                :format-control "Macro identifier-list accepts only a symbol, '~A' appeared."
                :format-arguments (list i))
    until (token-equal-p i ")")
    if (token-equal-p i "...")
      do (setf variadicp t)
    else if variadicp
           do (error 'preprocess-error
                     :format-control "Macro identifier-list has an extra identifier '~A' after '...'"
                     :format-arguments (list i))
    else if (or (token-equal-p i ",")
                (eql i +whitespace-marker+))
           do (progn)
    else
      collect i into identifier-list
    finally
       (return (values identifier-list variadicp token-list))))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|define|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (let* ((identifier
             (pop-preprocessor-directive-token directive-token-list directive-symbol))
           (next-token (first directive-token-list))
           (function-like-p (token-equal-p next-token '|(|)))
      (cond
        (function-like-p
         (multiple-value-bind (identifier-list variadicp rest-token-list)
             (collect-function-like-macro-identifier-list directive-token-list directive-symbol)
           (add-local-preprocessor-macro state identifier
                                         (make-instance 'function-like-macro
                                                        :name identifier
                                                        :identifier-list identifier-list :variadicp variadicp
                                                        :replacement-list rest-token-list))))
        (t
         (unless (or (null directive-token-list)
                     (eq next-token +whitespace-marker+))
           (warn 'with-c-syntax-style-warning
                 :message "C99 requires whitespace after the macro name."))
         (add-local-preprocessor-macro state identifier
                                       (make-instance 'object-like-macro
                                                      :name identifier
                                                      :replacement-list directive-token-list)))))))

;;; #undef

(defun remove-local-preprocessor-macro (state symbol)
  (deletef (pp-state-macro-alist state) symbol
           :key 'car :count 1))

(defmethod process-preprocessing-directive ((directive-symbol
                                             (eql 'with-c-syntax.preprocessor-directive:|undef|))
                                            directive-token-list state)
  (with-preprocessor-state-slots (state)
    (when if-section-skip-reason
      (return-from process-preprocessing-directive nil))
    (let ((identifier
            (pop-last-preprocessor-directive-token directive-token-list directive-symbol)))
      (remove-local-preprocessor-macro state identifier))))

;;; #line

(defun parse-line-arguments (token-list directive-symbol try-pp-macro-expand macro-alist pp-state)
  (let* ((tmp-token-list token-list)
         (line-number (pop-preprocessor-directive-token tmp-token-list directive-symbol))
         (line-number
           (typecase line-number
             (preprocessing-number
              (let ((pp-number-string (preprocessing-number-string line-number)))
                (unless (every 'digit-char-p pp-number-string)
                  (error 'preprocess-error
                         :format-control "#line argument '~A' has non-digit char."
                         :format-arguments (list pp-number-string))))
              (parse-preprocessing-number line-number))
             (integer line-number)
             (otherwise nil)))
         (file-name-supplied-p
           (preprocessor-token-exists-p tmp-token-list))
         (file-name
           (if file-name-supplied-p
               (pop-preprocessor-directive-token tmp-token-list directive-symbol))))
    (cond
      ((and (integerp line-number)
            (or (null file-name)
                (stringp file-name)
                (pathnamep file-name))
            (not (preprocessor-token-exists-p tmp-token-list)))
       (values line-number file-name file-name-supplied-p))
      (try-pp-macro-expand
       (let ((expanded (expand-each-preprocessor-macro-in-list token-list macro-alist pp-state)))
         (return-from parse-line-arguments
           (parse-line-arguments expanded directive-symbol nil nil nil))))
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
        (parse-line-arguments directive-token-list directive-symbol t macro-alist state)
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
    (let ((token-list-for-print (remove-whitespace-marker directive-token-list)))
      (error 'preprocess-error
             :format-control "[File ~A: Line ~D] #error: ~{~A~^ ~}"
             :format-arguments (list (if file-pathname (enough-namestring file-pathname)) 
                                     line-number
                                     token-list-for-print)))))

;;; #pragma

(defun process-with-c-syntax-pragma (directive-symbol directive-token-list)
  (let ((token1 (pop-preprocessor-directive-token directive-token-list directive-symbol)))
    (flet ((raise-unsyntactic-wcs-pragma-error ()
             (error 'preprocess-error
                    :format-control "Unsyntactic pragma '#pragma ~A ~A'"
                    :format-arguments (list :WITH_C_SYNTAX token1))))
      (switch (token1 :test 'token-equal-p)
        ;; Reader pragmas are also processed by `process-reader-pragma'.
        ("IN_PACKAGE"
         (let* ((package-designator
                  (pop-preprocessor-directive-token directive-token-list directive-symbol))
                (package (find-package package-designator)))
           (unless package
             (error 'preprocess-error
                    :format-control "No package named '~A'."
                    :format-arguments (list package-designator)))
           (check-no-preprocessor-token directive-token-list directive-symbol)
           (setf *package* package)))
        ("IN_WITH_C_SYNTAX_READTABLE"
         (let* ((token2
                  (pop-preprocessor-directive-token directive-token-list directive-symbol))
                (token3
                  (if (preprocessor-token-exists-p directive-token-list)
                      (pop-preprocessor-directive-token directive-token-list directive-symbol))))
           (multiple-value-bind (new-level new-case arg1 arg2)
               (parse-in-with-c-syntax-readtable-parameters token2 token3)
             (when (or arg1 arg2)
               (error 'preprocess-error
                      :format-control "Bad argument for '#pragma WITH_C_SYNTAX ~A: ~A ~@[~A~]'"
                      :format-arguments (list token1 token2 token3)))
             (check-no-preprocessor-token directive-token-list directive-symbol)
             (setf *readtable*
                   (find-c-readtable new-level new-case)))))
        (otherwise
         (raise-unsyntactic-wcs-pragma-error))))))


(defun process-stdc-pragma (directive-symbol directive-token-list state)
  (with-preprocessor-state-slots (state)
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
        (setf on-off-switch
              (switch (token2 :test 'token-equal-p)
                ("ON" t)
                ("OFF" nil)
                ("DEFAULT" :default)
                (otherwise (raise-unsyntactic-stdc-pragma-error))))
        (switch (token1 :test 'token-equal-p)
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
  (when (pp-state-if-section-skip-reason state)
    (return-from process-preprocessing-directive nil))
  (unless (preprocessor-token-exists-p directive-token-list)
    (warn 'with-c-syntax-style-warning
          :message "#pragma does not have any arguments.")
    (return-from process-preprocessing-directive nil))
  (let ((first-token
          (pop-preprocessor-directive-token directive-token-list directive-symbol)))
    (cond ((pp-with-c-syntax-pragma-p first-token)
           (process-with-c-syntax-pragma directive-symbol directive-token-list))
          ((pp-stdc-pragma-p first-token)
           (process-stdc-pragma directive-symbol directive-token-list state))
          ((pp-once-pragma-p first-token)
           (warn 'with-c-syntax-style-warning
                 :message "Sorry, '#pragma once' is under implementation."))
          (t
           (with-preprocessor-state-slots (state)
             (warn 'with-c-syntax-style-warning
                   :message (format nil "pragma '~A ~{~A~^ ~}' was ignored."
                                    first-token
                                    (remove-whitespace-marker directive-token-list))))))))

;;; Preprocessor loop helpers.

(defun preprocessor-loop-at-directive-p (state token)
  (with-preprocessor-state-slots (state)
    (and (zerop tokens-in-line)
         (or (token-equal-p token "#")
             (and process-digraph? (token-equal-p token "%:"))))))

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
                    (find-preprocessor-directive (symbol-name directive-token))))
            (process-preprocessing-directive directive-symbol token-list state)
            (raise-pp-error)))))))

(defun preprocessor-loop-do-pragma-operator (state)
  (with-preprocessor-state-slots (state)
    (unless (token-equal-p (pop-preprocessor-directive-token token-list :|_Pragma|) "(")
      (error 'preprocess-error
             :format-control "~A should be followed by '('"
             :format-arguments (list :|_Pragma|)))
    (let ((pragma-string (pop-preprocessor-directive-token token-list :|_Pragma|)))
      (unless (stringp pragma-string)
        (error 'preprocess-error
               :format-control "~A argument '~A' is not a string."
               :format-arguments (list :|_Pragma| pragma-string)))
      (unless (token-equal-p (pop-preprocessor-directive-token token-list :|_Pragma|) ")")
        (error 'preprocess-error
               :format-control "~A should be ended by ')'"
               :format-arguments (list :|_Pragma|)))
      (let ((pragma-tokens
              (with-input-from-string (stream pragma-string)
                (tokenize-source stream nil))))
        (process-preprocessing-directive 'with-c-syntax.preprocessor-directive:|pragma|
                                         pragma-tokens
                                         state)))))

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
        ((eq token :end-of-inclusion)
         (pop-include-state state))
        ((preprocessor-loop-at-directive-p state token) ; Check directives before #if skips.
         (preprocessor-loop-do-directives state))
        (if-section-skip-reason         ; #if-section skips it.
         (progn))
        (t
         (typecase token
           (symbol
            (mv-cond-let (it it-2)
              ((eq token :end-of-preprocessor-macro-scope)
               (pop macro-alist))
              ((pp-pragma-operator-p token)
               (preprocessor-loop-do-pragma-operator state))
              ;; Part of translation Phase 4 -- preprocessor macro
              ((preprocessor-macro-exists-p macro-alist token)
               (multiple-value-bind (expansion-list rest-tokens new-macro-alist not-invoked)
                   (expand-preprocessor-macro token token-list macro-alist state)
                 (cond (not-invoked
                        (push token result-list))
                       (t
                        (setf macro-alist new-macro-alist)
                        (setf token-list
                              (append expansion-list ; Rescan the expansion results.
                                      rest-tokens))))))
	      ;; Intern punctuators, includes digraphs.
              ((find-punctuator (symbol-name token) process-digraph?)
               (push it result-list))
	      ;; Intern keywords.
              ((find-c-terminal (symbol-name token))
               (push it result-list))
              ;; with-c-syntax specific: Try to split the token.
              ((and (let ((level (get-c-readtable-level *readtable*))) ; FIXME: cache current level for speed up.
                      (and level (<= level 1)))  
                    (not (or (boundp token)
                             (fboundp token)
                             (find-c-terminal (symbol-name token))))
                    (preprocessor-try-split token))
               (setf token-list (nconc it-2 token-list)))
              (t
               (push token result-list))))
           (string
            (preprocessor-loop-concatenate-string state token))
           (macro-definition ; This is not a token -- a marker for suppress recursive expansion.
            (push (cons (macro-definition-name token) :macro-suppressed)
                  macro-alist))
           (t
            (push token result-list)))
         (incf tokens-in-line))))))

(defun preprocessor (token-list reader-level readtable-case input-file-pathname
                     &key (process-digraph *with-c-syntax-preprocessor-process-digraph*))
  "This function preprocesses TOKEN-LIST before parsing.

Current workings are below:

- Interning a symbol into this package when it has a same name as C
  keywords or operators, or libc symbols.
  READTABLE-CASE affects how to do it.

- Concatenation of string literals.

- Expands preprocessor macros defined by '#defined'. (Undocumented now.)
"
  (let* ((pp-state
           (make-instance 'preprocessor-state
                          :token-list token-list
                          :file-pathname input-file-pathname 
                          :process-digraph? process-digraph))
         (*previous-readtable* *readtable*)
         (*readtable* (find-c-readtable reader-level readtable-case))
         (*package* *package*) ; Preserve `*package*' variable because it may be changed by pragmas.
         (pp-state
           (preprocessor-loop pp-state)))
    (nreverse (pp-state-result-list pp-state))))
