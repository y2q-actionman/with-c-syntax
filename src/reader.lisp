(in-package #:with-c-syntax.core)

(defconstant +with-c-syntax-default-reader-level+ 1)

(defvar *with-c-syntax-reader-level* +with-c-syntax-default-reader-level+
  "Holds the reader level used by '#{' reader function.
The value is one of 0, 1 (default), 2.

For inside '#{' and '}#', three syntaxes are defined. These syntaxes
are selected by the infix parameter of the '#{' dispatching macro
character. If it not specified, the value of this variable is used.

Available syntaxes are below:


* Level 0 (conservative)

In level 0, these reader macros are installed.

- ',' :: ',' is read as a symbol. (In ANSI CL, a comma is defined as
         an invalid char outside the backquote syntax.)
- ':' :: Reads a solely ':' as a symbol. Not a solely one (like
         `cl:cons') works as a normal package marker.
- '|' :: If '||' appeared, it becomes a symbol. The 'empty symbol'
  syntax is lost.  If '|' appeared followed by a terminating char, it
  becomes a single character symbol.  For using whitespace chars ahead
  of symbol, '\' can be used, like '\ abc'.

Level 0 is almost comatible with the standard syntax. However, we need
many escapes for using C operators.


* Level 1 (aggressive).

In level 1, these reader macros are installed.

- '{', '}', '[', ']' :: These become a terminating character, and read
  as a symbol.
- '`' :: '`' reads a next s-exp in `*previous-syntax*' readtable. This
  works as an escape from '#{' and '}#'. The 'backquote' functionality
  is lost.
- '.' :: Reads a solely '.' as a symbol. The 'consing dot'
  functionality is lost.
- '/' :: '//' means a line comment, '/* ... */' means a block
  comment. '/' is still non-terminating, and has special meanings only
  if followed by '/' or '*'. (Ex: 'a/b/c' or '/+aaa+/' are still valid
  symbols.)
- ''' :: The single-quote works as a character literal of C.  The
  `quote' functionality is lost.
- '\"' :: The double-quote works as a string literal of C. Especially,
  escaping is treated as C. The original functionality is lost.
- ';' :: ';' becomes a terminating character, and read as a
  symbol. The 'comment' functionality is lost.
- '(' and ')' :: parenthesis become a terminating character, and read
  as a symbol.  The 'reading a list' functionality is lost.

Level 1 overwrites macro characters in the standard syntax.  Only
constituent characters are left unchanged.  Especially, '(' and ')'
loses its functionalities. For constructing a list of Lisp, the '`'
syntax must be used.


* Level 2 (overkill)

In this level, these characters become terminating, and read as a
symbol listed below.

- '?' :: '?'
- '~' :: '~'
- ':' :: ':'
- '=' :: '=' or '=='
- '*' :: '*' or '*='
- '^' :: '^' or '^='
- '!' :: '!' or '!='
- '&' :: '&', '&&', or '&='
- '|' :: '|', '||', or '|='
- '+' :: '+', '++', or '+='
- '-' :: '-', '--', '-=', or '->'
- '>' :: '>', '>>', or '>>='
- '<' :: '<', '<<', or '<<='
- '/' :: '/', or '/='. '//' means a line comment, and '/* ... */'
         means a block comment.
- '.' :: '.' or a numeric literal of C language.

And, digit characters (0,1,2,3,4,5,6,7,8,9) are read as a C numeric
literals.

In this level, there is no compatibilities between symbols of Common
Lisp.  Especially, for denoting a symbol consists of terminating
characters, escapes are required. (ex. most\-positive\-fixnum)")

(defvar *with-c-syntax-reader-case* nil
  "Holds the readtable case used by '#{' reader function.

When this is not nil, it must be one of `:upcase', `:downcase',
`:preserve', or `:invert'.  The specified case is used as the
readtable-case inside '#{' and '}#' and passed to the
wrapping `with-c-syntax' form.

When this is nil, the `readtable-case' of the current `*readtable*' at
'#{' is used." )

(defvar *previous-syntax* (copy-readtable nil)
  "Holds the readtable used by #\` syntax.
This is bound by '#{' read macro to the `*readtable*' at that time.")


(defun read-in-previous-syntax (stream char)
  (declare (ignore char))
  (let ((*readtable* *previous-syntax*))
    (read stream t nil t)))

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (symbolicate char))

(defun terminating-char-p (char &optional (readtable *readtable*))
  "Returns T if char is terminating."
  (case char
    ((#\tab #\newline #\linefeed #\page #\return #\space)
     t)
    (otherwise
     (multiple-value-bind (fn non-terminating-p)
	 (get-macro-character char readtable)
       (and fn (not non-terminating-p))))))

(defun read-lonely-single-symbol (stream char)
  ;; I need this buffering to suppress calling `unread-char' too many times.
  (let ((buf (make-string 1 :element-type 'character :initial-element char))
        (next (peek-char nil stream t nil t)))
    (if (terminating-char-p next)
        (symbolicate buf)
        ;; For supporting 'a| |b'
        (with-input-from-string (buf-stream buf)
          (with-open-stream (in (make-concatenated-stream buf-stream stream))
            ;; TODO: restore old code which removes reader macro.
            (let ((*readtable* *previous-syntax*))
	      (read in t nil t))))))
  #+ ()
  (let ((buf (make-array '(1) :element-type 'character
			      :initial-contents `(,char)
			      :adjustable t :fill-pointer t)))
    (loop for c = (peek-char nil stream t nil t)
          until (terminating-char-p c)
          do (read-char stream t nil t)
             (vector-push-extend c buf))
    (if (length= 1 buf)
        (symbolicate buf)
        (let ((*readtable* *previous-syntax*))
	  (read-from-string buf)))))

(defun read-solely-bar (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= char next)            ; '||'
           (read-char stream t nil t)
           (symbolicate char next))
          (t
           (read-lonely-single-symbol stream char)))))

(defun read-slash-comment (stream char
                           &optional (next-function #'read-lonely-single-symbol))
  (case (peek-char nil stream t nil t)
    (#\/
     (read-line stream t nil t)
     (values))
    (#\*
     (read-char stream t nil t)
     (loop (peek-char #\* stream t nil t) ; skips until '*' char.
	(read-char stream t nil t)	  ; and eats the '*'
	(when (char= #\/ (read-char stream t nil t)) ; checks whether the next is '/' ?
	  (return)))
     (values))
    (otherwise
     (funcall next-function stream char))))

(defun read-universal-character-name (stream number-of-digits)
  "Reads \unnnn or \Unnnnnnnn syntax."
  (check-type number-of-digits (and fixnum (member 4 8)))
  (let* ((hexadecimals
           (with-output-to-string (out)
             (loop
               repeat number-of-digits
               for char = (read-char stream t nil t)
               unless (digit-char-p char 16)
                 do (error 'with-c-syntax-reader-error
                           :format-control "Character '~C' is not a hexadecimal for universal character names."
                           :format-arguments (list char))
               do (write-char char out))))
         (code (parse-integer hexadecimals :radix 16)))
    ;; See 6.4.3 Universal character names in ISO/IEC 9899:1999.
    (when (or (and (< code #x00A0)
                   (not (member code '(#x0024 #x0040 #x0060)))) ; $, @, `
              (<= #xD8000 code #xDFFF))
      (error 'with-c-syntax-reader-error
             :format-control "Universal character name '~A' is not usable."
             :format-arguments (list hexadecimals)))
    (code-char code)))

(defun read-numeric-escape (stream radix &key (limit most-positive-fixnum))
  "Used by `read-escaped-char'."
  (loop with tmp = 0
        for count from 0 below limit   ; Octal escape is limited for 3 characters.
	for c = (peek-char nil stream t nil t)
	as weight = (digit-char-p c radix)
	while weight
	do (read-char stream t nil t)
	   (setf tmp (+ (* tmp radix) weight))
	finally (return (code-char tmp))))

(defun read-escaped-char (stream)
  (let ((c0 (read-char stream t nil t)))
    (case c0
      ;; FIXME: This code assumes ASCII is used for char-code..
      (#\a #.(code-char #x07))          ; alarm
      (#\b #\Backspace)
      (#\f #\Page)
      (#\n #\Newline)
      (#\r #\Return)
      (#\t #\Tab)
      (#\v #.(code-char #x0b))          ; vertical tab
      (#\\ #\\)
      (#\' #\')
      (#\" #\")
      (#\? #\?)
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (unread-char c0 stream)
       (read-numeric-escape stream 8 :limit 3))
      (#\x
       (read-numeric-escape stream 16))
      (#\u
       (read-universal-character-name stream 4))
      (#\U
       (read-universal-character-name stream 8))
      (otherwise
       (error 'with-c-syntax-reader-error
              :format-control "Bad escaped char: ~C."
              :format-arguments (list c0))))))

(defun read-single-quote (stream &optional (c0 #\'))
  (declare (ignore c0))
  (let ((c1 (read-char stream t nil t)))
    (case c1
      (#\'
       (error 'with-c-syntax-reader-error
	      :format-control "Empty char constant."))
      (#\newline
       (error 'with-c-syntax-reader-error
	      :format-control "Char constant cannot contain a newline directly."))
      (#\\
       (setf c1 (read-escaped-char stream))))
    (let ((c2 (read-char stream t nil t)))
      (unless (char= c2 #\')
	(error 'with-c-syntax-reader-error
               :format-control "Too many chars appeared between '' :~C, ~C, ..."
               :format-arguments (list c1 c2))))
    c1))

(defun read-double-quote (stream &optional (c0 #\"))
  (declare (ignore c0))
  (with-output-to-string (out)
    (loop
      for c = (read-char stream t nil t)
      until (eql c #\")
      if (eql c #\\)
        do (write-char (read-escaped-char stream) out)
      else if (eql c #\newline)
        do (error 'with-c-syntax-reader-error
	          :format-control "String literal cannot contain a newline directly.")
      else
        do (write-char c out))))

(defun read-dot (stream char)
  "Dot may be an operator or a prefix of floating numbers."
  (let ((next (peek-char nil stream t nil t)))
    (if (digit-char-p next 10)
        (read-numeric-literal stream char)
        (read-single-character-symbol stream char))))

(defun read-single-or-compound-assign (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (case next
      (#\=
       (read-char stream t nil t)
       (symbolicate char next))
      (otherwise
       (read-single-character-symbol stream char)))))

(defun read-single-or-equal-or-self-symbol (stream char)
  "For '+', '&', '|'. They may be '+', '++', or '+='"
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next char)
	   (read-char stream t nil t)
	   (symbolicate char next))
	  (t
	   (read-single-or-compound-assign stream char)))))

(defun read-minus (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next #\>)
	   (read-char stream t nil t)
	   (symbolicate char next))
	  (t
	   (read-single-or-equal-or-self-symbol stream char)))))

(defun read-shift (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next char)	; shift op
	   (read-char stream t nil t)
	   (let ((next2 (peek-char nil stream t nil t)))
	     (case next2
	       (#\=
		(read-char stream t nil t)
		(symbolicate char next next2))
	       (otherwise
		(symbolicate char next)))))
	  (t
	   (read-single-or-compound-assign stream char)))))

(defun read-slash (stream char)
  (read-slash-comment stream char
                      #'read-single-or-compound-assign))

(defun read-preprocessing-number (stream c0)
  "Reads a preprocessing number token, defined in
  \"6.4.8 Preprocessing numbers\" in ISO/IEC 9899:1999."
  ;; pp-number:
  ;;   digit
  ;;   . digit
  ;;   pp-number digit
  ;;   pp-number identifier-nondigit
  ;;   pp-number e sign
  ;;   pp-number E sign
  ;;   pp-number p sign
  ;;   pp-number P sign
  ;;   pp-number .
  (with-output-to-string (out)
    (write-char c0 out)                 ; Write the first char.
    (loop for char = (read-char stream nil nil t)
          while char
          if (or (alphanumericp char)
                 (char= char #\_)
                 (char= char #\.))
            do (write-char char out)
               (case char
                 ((#\e #\E #\p #\P)
                  (let ((next (peek-char nil stream nil nil t)))
                    (case next
                      ((#\+ #\-)
                       (read-char stream t nil t)
                       (write-char next out))))))
          else
            if (char= char #\\)  ; May be an universal-character-name.
              do (let ((esc (read-char stream t nil t)))
                   (case esc
                     (#\u
                      (write-char (read-universal-character-name stream 4) out))
                     (#\U
                      (write-char (read-universal-character-name stream 8) out))
                     (otherwise
                      (error 'with-c-syntax-reader-error
                             :format-control "Bad escaped character in number: ~C."
                             :format-arguments (list esc)))))
          else
            do (unread-char char stream)
               (loop-finish))))

(defun find-numeric-literal-type (pp-number-string)
  "Looks PP-NUMBER-STRING and returns its radix (integer) and type (:float or :integer)."
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
              :format-control "~A contains a prefix as a numeric constants."
              :format-arguments (list pp-number-string))))))

(defun number-prefix-length (radix)
  (ecase radix
    ((10 8) 0)
    ((2 16) 2)))

(defun read-integer-constant (pp-number-string radix)
  (let ((string (make-array (length pp-number-string)
                            :element-type 'character :fill-pointer 0))
        (integer-suffix-exists nil)
        (index nil))
    (with-input-from-string (stream pp-number-string :start (number-prefix-length radix)
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
    (let ((integer-type (list '|int|)))
      (when integer-suffix-exists
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
               ("^[uU](|[lL]|ll|LL)$" pp-number-string :start index :sharedp t)
             (setf integer-type
                   (suffix-to-numeric-type l-suffix t)))
           (cl-ppcre:register-groups-bind (l-suffix u-suffix)
               ("^([lL]|ll|LL)([uU]?)$" pp-number-string :start index :sharedp t)
             (setf integer-type
                   (suffix-to-numeric-type l-suffix (not (length= 0 u-suffix)))))
           (error 'with-c-syntax-reader-error
                  :format-control "Integer constant '~A' contains invalid suffix '~A' (radix ~D)."
                  :format-arguments (list pp-number-string (subseq pp-number-string index))))))
      (values
       (parse-integer string :radix radix)
       integer-type))))

(defun find-lisp-type-by-c-floating-suffix (suffix)
  (cond ((or (null suffix) (string= suffix "")) 'double-float)
        ((string-equal suffix "f") 'single-float)
        ((string-equal suffix "l") 'long-float)
        (t (assert nil () "Unknown float suffix ~A" suffix))))

(defun read-decimal-floating-constant (pp-number-string)
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

(defun read-hexadecimal-floating-constant (pp-number-string)
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

(defun read-numeric-literal (stream c0)
  "Read a C numeric literal."
  (let ((pp-number (read-preprocessing-number stream c0)))
    (multiple-value-bind (radix floatp)
        (find-numeric-literal-type pp-number)
      (if floatp
          (ecase radix
            (10 (read-decimal-floating-constant pp-number))
            (16 (read-hexadecimal-floating-constant pp-number)))
          (read-integer-constant pp-number radix)))))

(defun install-c-reader (readtable level)
  "Inserts reader macros for C reader. Called by '#{' reader macro."
  (check-type level integer)
  (when (>= level 0)			; Conservative
    ;; Comma is read as a symbol.
    (set-macro-character #\, #'read-single-character-symbol nil readtable)
    ;; Enables solely ':' as a symbol.
    (set-macro-character #\: #'read-lonely-single-symbol t readtable)
    ;; Reads '||' and solely '|' as a symbol.
    (set-macro-character #\| #'read-solely-bar t readtable))
  (when (>= level 1) 			; Aggressive
    ;; brackets
    (set-macro-character #\{ #'read-single-character-symbol nil readtable)
    (set-macro-character #\} #'read-single-character-symbol nil readtable)
    (set-macro-character #\[ #'read-single-character-symbol nil readtable)
    (set-macro-character #\] #'read-single-character-symbol nil readtable)
    ;; for accessing normal syntax.
    (set-macro-character #\` #'read-in-previous-syntax nil readtable)
    ;; Disables 'consing dots', with replacement of ()
    (set-macro-character #\. #'read-lonely-single-symbol t readtable)
    ;; Destroying CL standard syntax -- overwrite standard macro chars.
    (set-macro-character #\/ #'read-slash-comment t readtable)
    (set-macro-character #\' #'read-single-quote nil readtable)
    (set-macro-character #\" #'read-double-quote nil readtable)
    (set-macro-character #\; #'read-single-character-symbol nil readtable)
    (set-macro-character #\( #'read-single-character-symbol nil readtable)
    (set-macro-character #\) #'read-single-character-symbol nil readtable))
  (when (>= level 2)			; Overkill
    ;; No compatibilities between CL symbols.
    (set-macro-character #\? #'read-single-character-symbol nil readtable)
    (set-macro-character #\~ #'read-single-character-symbol nil readtable)
    (set-macro-character #\: #'read-single-character-symbol nil readtable)
    (set-macro-character #\. #'read-dot nil readtable)
    (set-macro-character #\= #'read-single-or-compound-assign nil readtable)
    (set-macro-character #\* #'read-single-or-compound-assign nil readtable)
    (set-macro-character #\% #'read-single-or-compound-assign nil readtable)
    (set-macro-character #\^ #'read-single-or-compound-assign nil readtable)
    (set-macro-character #\! #'read-single-or-compound-assign nil readtable)
    (set-macro-character #\& #'read-single-or-equal-or-self-symbol nil readtable)
    (set-macro-character #\| #'read-single-or-equal-or-self-symbol nil readtable) ; TODO: Should I use Lisp escape for a symbol like '|assert|'?
    (set-macro-character #\+ #'read-single-or-equal-or-self-symbol nil readtable)
    (set-macro-character #\- #'read-minus nil readtable)
    (set-macro-character #\< #'read-shift nil readtable)
    (set-macro-character #\> #'read-shift nil readtable)
    (set-macro-character #\/ #'read-slash nil readtable)
    ;; Numeric litrals
    (set-macro-character #\0 #'read-numeric-literal t readtable)
    (set-macro-character #\1 #'read-numeric-literal t readtable)
    (set-macro-character #\2 #'read-numeric-literal t readtable)
    (set-macro-character #\3 #'read-numeric-literal t readtable)
    (set-macro-character #\4 #'read-numeric-literal t readtable)
    (set-macro-character #\5 #'read-numeric-literal t readtable)
    (set-macro-character #\6 #'read-numeric-literal t readtable)
    (set-macro-character #\7 #'read-numeric-literal t readtable)
    (set-macro-character #\8 #'read-numeric-literal t readtable)
    (set-macro-character #\9 #'read-numeric-literal t readtable))
  ;; TODO: If I support trigraphs or digraphs, I'll add them here.
  ;; (But I think these are not needed because the Standard characters include
  ;; the replaced characters/)
  ;; TODO: C99 support?
  ;; - An identifier begins with '\u' (universal character)
  ;;   How to be '\' treated?
  ;; - 'L' prefix of character literals.
  readtable)

(defun read-2chars-delimited-list (c1 c2 &optional stream recursive-p)
  "Used by `read-in-c-syntax' for reading '#{ ... }#' syntax."
  (loop for lis = (read-delimited-list c1 stream recursive-p)
     nconc lis
     until (char= c2 (peek-char nil stream t nil recursive-p))
     collect (symbolicate c1)	; assumes c1 is a kind of terminating.
     finally
       (assert (char= c2 (read-char stream t nil recursive-p))))) ; eat the peeked char.

(defun read-in-c-syntax (stream char n)
  "Called by '#{' reader macro of `with-c-syntax-readtable'.
Inside '#{' and '}#', the reader uses completely different syntax, and
the result is wrapped with `with-c-syntax'.
 See `*with-c-syntax-reader-level*' and `*with-c-syntax-reader-case*'."
  (assert (char= char #\{))
  (let* ((*previous-syntax* *readtable*)
         (*readtable* (copy-readtable))
         (*read-default-float-format* 'double-float) ; In C, floating literal w/o suffix is double.
         (level (alexandria:clamp (or n *with-c-syntax-reader-level*) 0 2)))
    (when *with-c-syntax-reader-case*
      (setf (readtable-case *readtable*) *with-c-syntax-reader-case*))
    (install-c-reader *readtable* level)
    `(with-c-syntax (:readtable-case ,(readtable-case *readtable*)) ; Capture the readtable-case used for reading inside '#{ ... }#'.
       ,@(read-2chars-delimited-list #\} #\# stream t))))

(defreadtable with-c-syntax-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'read-in-c-syntax))

;;; Sadly, `defreadtable' does not have docstring syntax..
;;; So, I added them into `read-in-c-syntax'.

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
