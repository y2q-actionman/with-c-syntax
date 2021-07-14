(in-package #:with-c-syntax.core)

(defvar *with-c-syntax-preprocessor-process-digraph* nil
  "Determines whether preprocessor replaces digraphs.
 If this is true, replacement occurs but `with-c-syntax-style-warning' is signalled.
 If this is `:no-warn', replacement occurs and the style-warning is not signalled.")

;;; Special preprocessor macro definitions

(defpackage #:with-c-syntax.preprocessor-special-macro
  (:use)
  (:export #:__DATE__ #:__FILE__ #:__LINE__ #:__TIME__))

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

;;; Preprocessor macro expansion

(defun va-args-identifier-p (token)
  (token-equal-p token :__VA_ARGS__))

(defclass object-like-macro ()
  ((name :initarg :name :reader object-like-macro-name)
   (replacement-list :initarg :replacement-list
                     :reader object-like-macro-replacement-list)))

(defmethod cl:print-object ((macro-definition object-like-macro) stream)
  (print-unreadable-object (macro-definition stream :type t :identity t)
    (princ (object-like-macro-name macro-definition) stream)))

(defmethod cl:initialize-instance :after ((macro-definition object-like-macro) &rest args)
  (when (va-args-identifier-p (object-like-macro-name macro-definition))
    (error 'preprocess-error
	   :format-control "Macro name '__VA_ARGS__' is now allowed as an user defined macro."))
  (when (some #'va-args-identifier-p (object-like-macro-replacement-list macro-definition))
    (error 'preprocess-error
	   :format-control "Object-like-macro cannot have '__VA_ARGS__' in its replacement-list.")))

(defclass function-like-macro ()
  ((name :initarg :name :reader function-like-macro-name)
   (identifier-list :initarg :identifier-list
                    :reader function-like-macro-identifier-list)
   (variadicp :initarg :variadicp
              :reader function-like-macro-variadicp)
   (replacement-list :initarg :replacement-list
                     :reader function-like-macro-replacement-list)))

(defmethod cl:print-object ((macro-definition function-like-macro) stream)
  (print-unreadable-object (macro-definition stream :type t :identity t)
    (princ (function-like-macro-name macro-definition) stream)
    (princ (function-like-macro-identifier-list macro-definition) stream)))

(defmethod cl:initialize-instance :after ((macro-definition function-like-macro) &rest args)
  (when (va-args-identifier-p (function-like-macro-name macro-definition))
    (error 'preprocess-error
	   :format-control "Macro name '__VA_ARGS__' is now allowed as an user defined macro."))
  (let ((identifier-list (function-like-macro-identifier-list macro-definition)))
    (when (some #'va-args-identifier-p identifier-list)
      (error 'preprocess-error
	     :format-control "Function-like-macro cannot have '__VA_ARGS__' in its identifier-list."))
    (unless (length= identifier-list (remove-duplicates identifier-list))
      (error 'preprocess-error
	     :format-control "Function-like-macro has duplicated parameter names.")))
  (when (and (not (function-like-macro-variadicp macro-definition))
             (some #'va-args-identifier-p (function-like-macro-replacement-list macro-definition)))
    (error 'preprocess-error
	   :format-control "Non-variadic Function-like-macro cannot have '__VA_ARGS__' in its replacement-list.")))

(defun preprocessor-macro-exists-p (macro-alist symbol)
  (or (find-symbol (symbol-name symbol) '#:with-c-syntax.preprocessor-special-macro)
      (when-let ((entry (assoc symbol macro-alist)))
        (not (eql (cdr entry) :macro-suppressed)))))

(defun remove-whitespace-marker (token-list)
  (remove +whitespace-marker+ token-list))

(defun delete-consecutive-whitespace-marker (token-list)
  (loop for cons = token-list then next
        for next = (cond
                     ((and (eql (first cons) +whitespace-marker+)
                           (eql (second cons) +whitespace-marker+))
                      (setf (cdr cons) (cddr cons))
                      cons)
                     (t
                      (cdr cons)))
        while cons)
  token-list)

(define-condition incompleted-macro-arguments-error (preprocess-error)
  ((token-list :initarg :token-list)))

(defstruct pp-macro-argument
  (identifier)
  (token-list)       ; Still holds `:end-of-preprocessor-macro-scope'.
  (token-list-expansion nil)
  (macro-alist))      ; The context for macro expansion.

(defun collect-preprocessor-macro-arguments (macro-definition token-list macro-alist)
  (unless token-list
    (error 'incompleted-macro-arguments-error :token-list token-list))
  (let ((begin
          ;; The previous check for TOKEN-LIST causes dead path.
          (locally #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
            (pop-preprocessor-directive-token token-list '<macro-expansion>))))
    (unless (token-equal-p begin "(")
      (error 'preprocess-error
	     :format-control "A symbol (~S) found between a preprocessor macro and the first '('"
	     :format-arguments (list begin))))
  (loop
    with macro-arg-results = nil
    with identifier-list = (function-like-macro-identifier-list macro-definition)
    with variadicp = (function-like-macro-variadicp macro-definition)
    with va-args-arg = (if variadicp
                           (make-pp-macro-argument :identifier :__VA_ARGS__
                                                   :token-list nil
                                                   :macro-alist nil))
    with variadic-arg-count = 0
    for i = (pop token-list) ; Use `cl:pop' for preserving `+whitespace-marker+'.
    if (token-equal-p i ")")
      do (loop-finish)
    unless (or i token-list)
      do (error 'incompleted-macro-arguments-error :token-list token-list)
    do (let* ((identifier (or (pop identifier-list)
                              (if variadicp
                                  :__VA_ARGS__
                                  (error 'preprocess-error
                                         :format-control "Too many arguments for macro '~A' tokens: ~A"
                                         :format-arguments (list macro-definition token-list)))))
              (marg
                (loop
                  named collect-one-arg
                  with count-end-of-preprocessor-macro-scope = 0
                  with nest-level of-type fixnum = 0
                  for j = i then (pop token-list)
                  do (switch (j :test 'token-equal-p)
                       (:end-of-preprocessor-macro-scope
                        (incf count-end-of-preprocessor-macro-scope))
                       ("("
                        (incf nest-level))
                       (","
                        (loop-finish))
                       (")"
                        (when (minusp (decf nest-level))
	                  (push j token-list)
	                  (loop-finish))))
                  collect j into tokens
                  finally
                     (let ((pp-macro-arg
                             (make-pp-macro-argument :identifier identifier
                                                     :token-list tokens
                                                     :macro-alist macro-alist)))
                       (setf macro-alist
                             (nthcdr count-end-of-preprocessor-macro-scope macro-alist))
                       (return-from collect-one-arg pp-macro-arg)))))
         (cond
           ((eq identifier :__VA_ARGS__)
            (incf variadic-arg-count)
            (appendf (pp-macro-argument-token-list va-args-arg)
                     (list 'with-c-syntax.punctuator:\,)
                     (pp-macro-argument-token-list marg))
            (when (null (pp-macro-argument-macro-alist va-args-arg))
              (setf (pp-macro-argument-macro-alist va-args-arg)
                    (pp-macro-argument-macro-alist marg))))
           (t
            (push marg macro-arg-results))))
    finally
       (when variadicp
         (when (zerop variadic-arg-count)
           (warn 'with-c-syntax-style-warning
                 :message "Variadic macro does not have any arguments. __VA_ARGS__ bound to nil."))
         (push va-args-arg macro-arg-results))
       (return
         (values (nreverse macro-arg-results) token-list))))

(defun expand-macro-arguments (pp-macro-argument pp-state)
  (let ((token-list (pp-macro-argument-token-list pp-macro-argument))
        (macro-alist (pp-macro-argument-macro-alist pp-macro-argument)))
    (loop for head = (pop token-list)
          while (or head token-list)
          if (and (symbolp head)
                  (preprocessor-macro-exists-p macro-alist head))
            append (multiple-value-bind (expansion rest-tokens)
                       (expand-preprocessor-macro head token-list macro-alist pp-state)
                     (setf token-list rest-tokens)
                     expansion)
              into all-expansions
          else if (eql head :end-of-preprocessor-macro-scope)
                 do (pop macro-alist)
          else
            collect head into all-expansions
          finally
             (setf all-expansions (delete-consecutive-whitespace-marker all-expansions)
                   (pp-macro-argument-token-list-expansion pp-macro-argument) all-expansions)
             (return all-expansions))))

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
          with expansion = (pp-macro-argument-token-list-expansion marg)
          for lis on (if (eql (first expansion) +whitespace-marker+)
                         (rest expansion) ; Remove the first `+whitespace-marker+'.
                         expansion)
          as i = (car lis)
          do (cond
               ((and (eql i +whitespace-marker+)
                     (not (endp (rest lis)))) ; Do not print the last `+whitespace-marker+'.
                (write-char #\space out))
               ((characterp i)
                (format out "'~C'" i))
               ((stringp i)  
                (format out "\\\"")
                (loop for c across i
                      if (or (char= c #\") (char= c #\\))
                        do (write-char #\\ out) ; escape double-quote and backslash
                      do (write-char #\\))
                (format out "\\\""))
               (t
                (princ i out))))))

(defconstant +placemaker-token+ :||)

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
     (intern-to-its-package (concatenate 'string (symbol-name token1) (symbol-name token2))
                            token1))))

(defun stringify-conced-integer (int)
  (with-standard-io-syntax
    (princ-to-string int)))

(defmethod concatenate-token ((token1 symbol) (token2 integer))
  (intern-to-its-package (concatenate 'string (symbol-name token1)
                                      (stringify-conced-integer token2))
                         token1))

(defmethod concatenate-token ((token1 integer) (token2 symbol))
  (intern-to-its-package (concatenate 'string
                                      (stringify-conced-integer token1)
                                      (symbol-name token2))
                         token2))

(defun intern-conc-pp-number (str1 str2 package)
  (let* ((conc (concatenate 'string str1 str2)))
    (handler-case
        (read-preprocessing-number-from-string conc)
      (with-c-syntax-reader-error ()
        (intern conc package)))))

(defmethod concatenate-token ((token1 integer) (token2 integer))
  ;; Firstly I thought this can implement with numeric operatons, but
  ;; how to treat '10 ## -10' ?
  (intern-conc-pp-number (stringify-conced-integer token1)
                         (stringify-conced-integer token2)
                         *package*))

(defmethod concatenate-token ((token1 preprocessing-number) (token2 preprocessing-number))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (preprocessing-number-string token2)
                         *package*))

(defmethod concatenate-token ((token1 preprocessing-number) (token2 symbol))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (symbol-name token2)
                         (symbol-package token2)))

(defmethod concatenate-token ((token1 symbol) (token2 preprocessing-number))
  (intern-conc-pp-number (symbol-name token1)
                         (preprocessing-number-string token2)
                         (symbol-package token1)))
                         
(defmethod concatenate-token ((token1 preprocessing-number) (token2 integer))
  (intern-conc-pp-number (preprocessing-number-string token1)
                         (stringify-conced-integer token2)
                         *package*))

(defmethod concatenate-token ((token1 integer) (token2 preprocessing-number))
  (intern-conc-pp-number (stringify-conced-integer token1)
                         (preprocessing-number-string token2)
                         *package*))

(defun expand-concatenate-operator (token1 token2 macro-arg-alist)
  (flet ((expand-arg-token (token)
           (if-let ((entry (assoc token macro-arg-alist)))
             (pp-macro-argument-token-list-expansion (cdr entry))
             (list token))))
    (let* ((token1-expansion (or (expand-arg-token token1) (list +placemaker-token+)))
           (token2-expansion (or (expand-arg-token token2) (list +placemaker-token+)))
           (token1-last-whitespacep
             (eql (lastcar token1-expansion) +whitespace-marker+))
           (token1-last (if token1-last-whitespacep
                            (car (last token1-expansion 2))
                            (lastcar token1-expansion)))
           (token2-first-whitespacep
             (eql (first token2-expansion) +whitespace-marker+))
           (token2-first (if token2-first-whitespacep
                             (second token2-expansion)
                             (first token1-expansion)))
           (concat-result (concatenate-token
                           token1-last
                           token2-first)))
      (delete-consecutive-whitespace-marker
       (append (butlast token1-expansion
                        (if token1-last-whitespacep 2 1))
               (list concat-result)
               (nthcdr (if token2-first-whitespacep 2 1)
                       token2-expansion))))))

(defun expand-macro-replacement-list (replacement-list macro-arg-alist)
  (when (token-equal-p (first replacement-list) "##")
    (error 'preprocess-error
           :format-control "The first operand of '##' does not exists."))
  (loop
    for token = (pop replacement-list)
    as next-token = (let ((fst (first replacement-list)))
                      (if (eql fst +whitespace-marker+)
                          (second replacement-list)
                          fst))
    as macro-arg-entry = (if after-concatenation
                             nil
                             (assoc token macro-arg-alist))
    as after-concatenation = nil
    while (or token replacement-list)
    if (token-equal-p next-token "##")  ; Concatenation.
      do (when (eql (first replacement-list) +whitespace-marker+)
           (pop replacement-list))
         (assert (token-equal-p (pop replacement-list) "##"))
      and append (let* ((token2 (pop replacement-list))
                        (token2 (if (eql token2 +whitespace-marker+)
                                    (pop replacement-list)
                                    token2))
                        (conc-result-list
                          (expand-concatenate-operator token token2 macro-arg-alist)))
                   (push (lastcar conc-result-list) replacement-list)
                   (setf after-concatenation t)
                   (butlast conc-result-list))
            into expansion
    else if (token-equal-p token "#")   ; Stringify.
           do (when (eql (first replacement-list) +whitespace-marker+)
                (pop replacement-list))
              (assert (eq (pop replacement-list) next-token))
           and collect (expand-stringify-operator next-token macro-arg-alist)
                 into expansion
    else if macro-arg-entry             ; Replacement
           append (pp-macro-argument-token-list-expansion (cdr macro-arg-entry))
             into expansion
    else if (eq token +placemaker-token+)
           do (error 'preprocess-error
                     :format-control "Placemaker-token should not be left after macro expansion.")
    else 
      collect token into expansion
    finally
    (return (delete-consecutive-whitespace-marker expansion))))

(defun expand-function-like-macro (macro-definition rest-token-list macro-alist pp-state)
  "See `expand-preprocessor-macro'"
  ;; Collect arguments.
  (multiple-value-bind (macro-arg-list tail-of-rest-token-list)
      (collect-preprocessor-macro-arguments macro-definition rest-token-list macro-alist)
    ;; Pop used tokens from rest-token-list
    (setf rest-token-list tail-of-rest-token-list)
    ;; Expand arguments
    (let ((macro-arg-alist
            (loop for marg in macro-arg-list
                  do (expand-macro-arguments marg pp-state)
                  collect (cons (pp-macro-argument-identifier marg) marg))))
      (values 
       (expand-macro-replacement-list (function-like-macro-replacement-list macro-definition)
                                      macro-arg-alist)
       rest-token-list))))

(defun expand-object-like-macro (macro-definition)
  (expand-macro-replacement-list (object-like-macro-replacement-list macro-definition)
                                 nil))

(defun expand-preprocessor-macro (token rest-token-list macro-alist pp-state)
  "Expand preprocessor macro named TOKEN. If it is a function-like
macro, its arguments are taken from REST-TOKEN-LIST. MACRO-ALIST is an
alist of preprocessor macro definitions.  PP-STATE is a
`preprocessor-state' object, used for expanding special macros.

 Returns three values:
 1. A list of tokens made by expansion.
 2. A list, tail of REST-TOKEN-LIST, left after collecting function-like macro arguments.
 3. A cons of MACRO-ALIST which is used here. "
  (when-let ((special-macro
              (find-symbol (symbol-name token) '#:with-c-syntax.preprocessor-special-macro)))
    (return-from expand-preprocessor-macro
      (values (list (funcall special-macro pp-state))
              rest-token-list
              nil)))
  (let* ((pp-macro-entry (assoc token macro-alist))
         (pp-macro (cdr pp-macro-entry)))
    (etypecase pp-macro
      (function-like-macro
       (multiple-value-bind (expansion tail-token-list)
           (expand-function-like-macro pp-macro rest-token-list macro-alist pp-state)
         (values expansion
                 tail-token-list
                 pp-macro-entry)))
      (object-like-macro
       (let ((expansion (expand-object-like-macro pp-macro)))
         (values expansion
                 rest-token-list
                 pp-macro-entry))))))


;;; Identifier split.

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
  ((reader-level :initarg :reader-level :initform *with-c-syntax-reader-level* :type integer
                 :reader pp-state-reader-level)
   (readtable-case :initarg :readtable-case :type keyword
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
   (macro-alist :initform () :type list
                :accessor pp-state-macro-alist)
   (if-section-stack :initform nil :type list
                     :accessor pp-state-if-section-stack)
   (if-section-skip-reason :initform nil
                           :accessor pp-state-if-section-skip-reason)
   (include-stack :initform nil
                  :accessor pp-state-include-stack))
  (:default-initargs
   :readtable-case (or *with-c-syntax-reader-case*
                       (readtable-case *readtable*))))

;; TODO: reduce its usage
(defmacro with-preprocessor-state-slots ((state) &body body)
  `(with-accessors ((reader-level pp-state-reader-level)
                    (readtable-case pp-state-readtable-case)
                    (process-digraph? pp-state-process-digraph?)
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
                                          directive-symbol
                                          (pp-state-macro-alist state)))
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
         ;; TODO: merge with #if part.
         (unless (preprocessor-token-exists-p directive-token-list)
           (raise-no-preprocessor-token-error directive-symbol))
         (let* ((condition directive-token-list)
                (lexer (pp-if-expression-lexer directive-token-list process-digraph? readtable-case
                                               directive-symbol
                                               (pp-state-macro-alist state)))
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

;;; #include

(defun parse-header-name (token-list directive-symbol &key (try-pp-macro-expand t))
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
            (loop for token = (pop token-list)
                  until (token-equal-p token ">")
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

(defun tokenize-included-source-file (header-name state)
  (let* ((readtable-case (pp-state-readtable-case state))
         (line-sym (ecase readtable-case
                     ((:upcase :invert) '|LINE|)
                     ((:downcase :preserve) '|line|)))
         (pragma-sym (ecase readtable-case
                       ((:upcase :invert) '|PRAGMA|)
                       ((:downcase :preserve) '|pragma|)))
         (start-line-tokens
           (list '|#| line-sym 1 header-name +newline-marker+))
         (main-tokens
           (with-open-file (stream header-name)
             (tokenize-source (pp-state-reader-level state) stream nil)))
         (end-pragma-tokens
           (list +newline-marker+
                 '|#| pragma-sym :WITH_C_SYNTAX :END_OF_INCLUSION +newline-marker+))
         (end-line-tokens
           (if (pp-state-file-pathname state)
               (list '|#| line-sym (pp-state-line-number state) (pp-state-file-pathname state) +newline-marker+)
               (list '|#| line-sym (pp-state-line-number state) +newline-marker+))))
    (nconc start-line-tokens
           main-tokens
           end-pragma-tokens
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
        (parse-header-name directive-token-list directive-symbol)
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

(defun add-local-preprocessor-macro (state symbol value)
  ;; TODO: check redefinition.
  (push (cons symbol value)
        (pp-state-macro-alist state)))

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
    until (string= i ")")
    if (string= i "...")
      do (setf variadicp t)
    else if variadicp
           do (error 'preprocess-error
                     :format-control "Macro identifier-list has an extra identifier '~A' after '...'"
                     :format-arguments (list i))
    else if (or (string= i ",")
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
           (function-like-p (token-equal-p (first directive-token-list) '|(|)))
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

(defun parse-line-arguments (token-list directive-symbol &key (try-pp-macro-expand t))
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
    (let ((token-list-for-print (remove-whitespace-marker directive-token-list)))
      (error 'preprocess-error
             :format-control "[File ~A: Line ~D] #error: ~{~A~^ ~}"
             :format-arguments (list (if file-pathname (enough-namestring file-pathname)) 
                                     line-number
                                     token-list-for-print)))))

;;; #pragma

(defun process-with-c-syntax-pragma (directive-symbol directive-token-list state)
  (let ((token1 (pop-preprocessor-directive-token directive-token-list directive-symbol)))
      (flet ((raise-unsyntactic-wcs-pragma-error ()
               (error 'preprocess-error
                      :format-control "Unsyntactic pragma '#pragma ~A ~A'"
                      :format-arguments (list :WITH_C_SYNTAX token1))))
        (switch (token1 :test 'token-equal-p)
          (:END_OF_INCLUSION
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
      (switch (first-token :test 'token-equal-p)
        (:WITH_C_SYNTAX
         (process-with-c-syntax-pragma directive-symbol directive-token-list state))
        ("STDC"
         (process-stdc-pragma directive-symbol directive-token-list state))
        ;; TODO: #pragma once
        (otherwise
         (process-unknown-pragma))))))

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
        ((eq token :end-of-preprocessor-macro-scope)
         (pop macro-alist))
        ((preprocessor-loop-at-directive-p state token) ; Check directives before #if skips.
         (preprocessor-loop-do-directives state))
        (if-section-skip-reason         ; #if-section skips it.
         (progn))
        (t
         (typecase token
           (symbol
            (cond
              ;; Part of translation Phase 4 -- preprocessor macro
              ((preprocessor-macro-exists-p macro-alist token)
               (multiple-value-bind (expansion-list rest-tokens pp-macro-cons)
                   (expand-preprocessor-macro token token-list macro-alist state)
                 (when pp-macro-cons
                   (push (cons (car pp-macro-cons) :macro-suppressed)
                         macro-alist))
                 (setf token-list
                       (append expansion-list ; Rescan the expansion results.
                               (if pp-macro-cons
                                   (list :end-of-preprocessor-macro-scope))
                               (if (and (eql (lastcar expansion-list) +whitespace-marker+)
                                        (eql (first rest-tokens) +whitespace-marker+))
                                   (rest rest-tokens)
                                   rest-tokens)))))
       
	      ;; TODO: intern digraph here
	      
              ;; with-c-syntax specific: Try to split the token.
              ;; FIXME: I think this should be only for reader level 1.
              ((unless (or (boundp token)
                           (fboundp token)
                           (find-c-terminal (symbol-name token) readtable-case))
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

(defun preprocessor (token-list
                     &key (reader-level *with-c-syntax-reader-level*)
                       (readtable-case (or *with-c-syntax-reader-case*
                                           (readtable-case *readtable*)))
                       (input-file-pathname nil)
                     (process-digraph *with-c-syntax-preprocessor-process-digraph*))
  "This function preprocesses TOKEN-LIST before parsing.

Current workings are below:

- Interning a symbol into this package when it has a same name as C
  keywords or operators, or libc symbols.
  READTABLE-CASE affects how to do it.

- Concatenation of string literals.

- Expands preprocessor macros defined by '#defined'. (Undocumented now.)


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
                          :reader-level reader-level
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
