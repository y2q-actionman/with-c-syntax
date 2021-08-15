(in-package #:with-c-syntax.core)

;;; Numeric literal

(defun find-numeric-literal-type (pp-number-string)
  "Looks PP-NUMBER-STRING and returns its radix (integer) and a boolean whether its type is float or not."
  (flet ((find-decimal-float-marker ()
           (find-if (lambda (c) (member c '(#\. #\e #\E))) pp-number-string))
         (find-hexadecimal-float-marker ()
           (find-if (lambda (c) (member c '(#\. #\p #\P))) pp-number-string)))
    ;; See prefix.
    (case (char pp-number-string 0)
      (#\.
       (values 10 t)) ; fractional-constant, a part of decimal-floating-constant.
      (#\0
       (if (length= 1 pp-number-string) ; '0'
           (values 8 nil)
           (case (char pp-number-string 1)
             ((#\b #\B)                 ; binary-constant. Since C23.
              (values 2 nil))
             ((#\x #\X) ; hexadecimal-constant or hexadecimal-floating-constant.
              (values 16 (find-hexadecimal-float-marker)))
             (otherwise
              ;; May be octal (like '007' or '0u'), or decimal float ('0.').
              (if (find-decimal-float-marker)
                  (values 10 t)
                  (values 8 nil))))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ; decimal-constant or decimal-floating-constant
       (values 10 (find-decimal-float-marker)))
      (otherwise       ; This is a bug of `read-preprocessing-number'.
       (error 'with-c-syntax-reader-error
              :format-control "~A contains an unknown prefix as a numeric constant."
              :format-arguments (list pp-number-string))))))

(defun parse-integer-suffix (pp-number-string suffix-start-index)
  (flet ((suffix-to-numeric-type (l-suffix u-suffix)
           "See `+numeric-types-alist+'"
           `(,@(ecase (length l-suffix)
                 (0 nil)
                 (1 '(|long|))
                 (2 '(|long| |long|)))
               ,@(if u-suffix
                     '(|unsigned|)))))
    (or
     (cl-ppcre:register-groups-bind (l-suffix)
         ("^[uU](|[lL]|ll|LL)$" pp-number-string :start suffix-start-index :sharedp t)
       (suffix-to-numeric-type l-suffix t))
     (cl-ppcre:register-groups-bind (l-suffix u-suffix)
         ("^([lL]|ll|LL)([uU]?)$" pp-number-string :start suffix-start-index :sharedp t)
       (suffix-to-numeric-type l-suffix (not (length= 0 u-suffix))))
     (error 'with-c-syntax-reader-error
            :format-control "Integer constant '~A' contains invalid suffix '~A'."
            :format-arguments (list pp-number-string
                                    (subseq pp-number-string suffix-start-index))))))

(defun parse-integer-constant (pp-number-string radix)
  (let ((string (make-array (length pp-number-string)
                            :element-type 'character :fill-pointer 0))
        (number-prefix-length (ecase radix
                                ((10 8) 0)
                                ((2 16) 2)))
        (integer-suffix-exists nil)
        (index nil))
    (with-input-from-string (stream pp-number-string :start number-prefix-length
                                                     :index index)
      (loop for c = (read-char stream nil nil)
            while c
            do (cond
                 ((digit-char-p c radix)
                  (vector-push c string))
                 ((member c '(#\u #\U #\l #\L)) ; Integer suffixes
                  (setf integer-suffix-exists t)
                  (unread-char c stream)
                  (loop-finish))
                 (t
                  (error 'with-c-syntax-reader-error
                         :format-control "Integer constant '~A' contains invalid char '~C' (radix ~D)."
                         :format-arguments (list pp-number-string c radix))))))
    (let ((integer-type
            (if integer-suffix-exists
                (parse-integer-suffix pp-number-string index)
                (list '|int|))))
      (values
       (parse-integer string :radix radix)
       integer-type))))

(defun find-lisp-type-by-c-floating-suffix (suffix)
  (cond ((or (null suffix) (string= suffix "")) 'double-float)
        ((string-equal suffix "f") 'single-float)
        ((string-equal suffix "l") 'long-float)
        (t (assert nil () "Unknown float suffix ~A" suffix))))

(defun parse-decimal-floating-constant (pp-number-string)
  ;; See '6.4.4.2 Floating Constants' in ISO/IEC 9899:1999.
  (flet ((read-decimal-float (fractional exponent suffix)
           (let ((lisp-float-string
                   (format nil "~A~C~A"
                           fractional
                           (ecase (find-lisp-type-by-c-floating-suffix suffix)
                             (single-float #\f)
                             (double-float #\d)
                             (long-float #\l))
                           exponent)))
             (with-standard-io-syntax
               (read-from-string lisp-float-string)))))
    (or
     (cl-ppcre:register-groups-bind (fractional exponent suffix)
         ("^([0-9]*\\.[0-9]+|[0-9]+\\.)(?:[eE]([+-]?[0-9]+)|(?:))([flFL]?)$"
          pp-number-string :sharedp t)
       (read-decimal-float fractional
                           (if (or (null exponent) (string= exponent ""))
                               "0"
                               exponent)
                           suffix))
     (cl-ppcre:register-groups-bind (fractional exponent suffix)
         ("^([0-9]+)[eE]([+-]?[0-9]+)([flFL]?)$"
          pp-number-string :sharedp t)
       (read-decimal-float fractional exponent suffix))
     (error 'with-c-syntax-reader-error
            :format-control "Decimal floating constant '~A' cannot be read."
            :format-arguments (list pp-number-string)))))

(defun parse-hexadecimal-floating-constant (pp-number-string)
  ;; See '6.4.4.2 Floating Constants' in ISO/IEC 9899:1999.
  (flet ((read-hex-float (int-part frac-part exponent suffix)
           (let* ((prototype (coerce 1 (find-lisp-type-by-c-floating-suffix suffix)))
                  (significand-string (format nil "~A~A" int-part frac-part)) ; scales frac-part to integer.
                  (significand-int (parse-integer significand-string :radix 16))
                  (frac-part-length (length frac-part))
                  (exp-num (+ (if exponent (parse-integer exponent :radix 10) 0)
                              (* -4 frac-part-length)))) ; Decrease exponent corresponding to the scaling above.
             ;; Inverse of `integer-decode-float'. See the Hyperspec.
             (scale-float (float significand-int prototype)
                          exp-num))))
    (or
     (cl-ppcre:register-groups-bind (int-part frac-part exponent suffix)
         ("^0[xX]([0-9a-fA-F]*)\\.([0-9a-fA-F]+)[pP]([+-]?[0-9]+)([flFL]?)$"
          pp-number-string :sharedp t)
       (read-hex-float int-part frac-part exponent suffix))
     (cl-ppcre:register-groups-bind (int-part exponent suffix)
         ("^0[xX]([0-9a-fA-F]+)\\.?[pP]([+-]?[0-9]+)([flFL]?)$"
          pp-number-string :sharedp t)
       (read-hex-float int-part "" exponent suffix))
     (error 'with-c-syntax-reader-error
            :format-control "Hexadecimal floating constant '~A' cannot be read."
            :format-arguments (list pp-number-string)))))

(defun parse-preprocessing-number (preprocessing-number)
  "Read a C numeric literal from `preprocessing-number'"
  (with-accessors ((pp-number-string preprocessing-number-string)
                   (parse-result preprocessing-number-parse-result))
      preprocessing-number
    (unless parse-result
      (setf parse-result
            (multiple-value-bind (radix floatp)
                (find-numeric-literal-type pp-number-string)
              (if floatp
                  (ecase radix
                    (10 (parse-decimal-floating-constant pp-number-string))
                    (16 (parse-hexadecimal-floating-constant pp-number-string)))
                  (parse-integer-constant pp-number-string radix)))))
    parse-result))

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
