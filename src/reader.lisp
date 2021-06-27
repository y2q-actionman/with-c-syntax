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

Level 0 is almost comatible with the standard syntax. However, we need
many escapes for using C operators.


* Level 1 (aggressive).

In level 1, these reader macros are installed.

- '0' :: If followed by 'x' or 'X', hexadecimal integer (like
  '0xdeadbeef') and hexadecimal floating number (like '0x1.ffp1')
  syntax are enabled. Otherwise, it is treated as a normal token of
  Common Lisp.
- '|' :: If '||' appeared, it becomes a symbol. The 'empty symbol'
  syntax is lost.  If '|' appeared followed by a terminating char, it
  becomes a single character symbol. Because '|' becomes a macro character,
  it cannot be used for a multiple escape inside a symbol notation.
  (e.g. Use '|A B C|' instead of 'a| |b| |c').
- '{', '}', '[', ']' :: These become a terminating character, and read
  as a symbol.
- '`' :: '`' reads a next s-exp in `*previous-syntax*' readtable. This
  works as an escape from '#{' and '}#'. The 'backquote' functionality
  is lost.
- '.' :: Reads a solely '.' as a symbol. The 'consing dot'
  functionality is lost.
- '/' :: '//' means a line comment, '/* ... */' means a block
  comment. '/' is still non-terminating and it has special meanings only
  if followed by '/' or '*'. (Ex: 'a/b/c' or '/+aaa+/' are still valid
  symbols.)
- '\"' :: The double-quote works as a string literal of C. Especially,
  escaping is treated as C. The original functionality is lost.
- ';' :: ';' becomes a terminating character, and read as a
  symbol. The 'comment' functionality is lost.
- '(' and ')' :: parenthesis become a terminating character, and read
  as a symbol.  The 'reading a list' functionality is lost.

Level 1 still mostly keeps the syntax of symbols, since many
constituent characters are left unchanged.  But some major macro
characters are overwritten.  Especially, '(' and ')' loses its
functionalities. For constructing a list of Lisp, the '`' syntax must
be used.


* Level 2 (overkill)

In this level, these characters become terminating, and read as a
symbol listed below.

- '?' :: '?'
- '~' :: '~'
- ':' :: ':' or a (digraph ':>')
- '=' :: '=' or '=='
- '*' :: '*' or '*='
- '^' :: '^' or '^='
- '!' :: '!' or '!='
- '&' :: '&', '&&', or '&='
- '|' :: '|', '||', or '|='
- '+' :: '+', '++', or '+='
- '-' :: '-', '--', '-=', or '->'
- '>' :: '>', '>>', '>=', or '>>='
- '<' :: '<', '<<', '<=', '<<=', or digraphs ('<:', '<%')
- '/' :: '/', or '/='. '//' means a line comment, and '/* ... */'
         means a block comment.
- '.' :: '.', '...', or a numeric literal of C language.
- '%' :: '%', '%=', or digraphs ('%>', '%:', '%:%:')

And, these characters are changed:

- Digit characters (0,1,2,3,4,5,6,7,8,9) are read as a C numeric
  literals.
- The single-quote (') works as a character literal of C. The `quote'
  functionality is lost.

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

(defvar *second-unread-char* nil
  "Holds a character wanted to `unread-char' secondarily. This is used
 for implementing '...' and '%:%:'.")


(defun read-in-previous-syntax (stream char)
  (when (eq *readtable* *previous-syntax*)
    (error 'with-c-syntax-reader-error
           :stream stream
           :format-control "read-in-previous-syntax used recursively by char ~C"
           :format-arguments (list char)))
  (let ((*readtable* *previous-syntax*))
    (read-preserving-whitespace stream t nil t)))

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (symbolicate char))

(defun read-after-unread (unread-char stream &optional eof-error-p eof-value recursive-p)
  ;; I need this buffering to suppress calling `unread-char' too many times.
  (let ((buf (make-string 1 :element-type 'character :initial-element unread-char)))
    (declare (dynamic-extent buf))
    (with-input-from-string (buf-stream buf)
      (with-open-stream (conc-stream (make-concatenated-stream buf-stream stream))
        (read-preserving-whitespace conc-stream eof-error-p eof-value recursive-p)))))

(defun read-lonely-single-symbol (stream char)
  "If the next character in STREAM is terminating, returns a symbol made of CHAR.
If not, returns a next token by `cl:read' after unreading CHAR." 
  ;; See only one character to keep escapes and spaces (consider '|a b|').
  (let ((next (peek-char nil stream nil nil t)))
    (if (or (null next)
            (terminating-char-p next))
        (symbolicate char)
        (let ((*readtable* (copy-readtable nil))) ; Use the standard syntax.
          (read-after-unread char stream t nil t)))))

(defun read-solely-bar (stream char)
  (let ((next (peek-char nil stream nil nil t)))
    (cond ((eql char next)              ; '||'
           (read-char stream t nil t)
           (symbolicate char next))
          (t
           (read-lonely-single-symbol stream char)))))

(defconstant +wcs-end-marker+ '}#)

(defun read-right-curly-bracket (stream char)
  (case (peek-char nil stream nil nil t)
    (#\#                                ; '}#'
     (read-char stream t nil t)
     +wcs-end-marker+)
    (t
     (read-single-character-symbol stream char))))

(defun read-slash-comment (stream char
                           &optional (next-function #'read-lonely-single-symbol))
  ;; We count skipped newlines into the stream
  ;; (`physical-source-input-stream')  or return it, for __LINE__.
  (case (peek-char nil stream nil nil t)
    (#\/
     (read-line stream t nil t)
     +newline-marker+)
    (#\*
     (read-char stream t nil t)
     (loop
       for c = (read-char stream t nil t)
       if (and (eql c #\*)
               (char= #\/ (peek-char nil stream t nil t)))
         do (read-char stream t nil t)
            (loop-finish)
       if (eql c #\newline)
         count it into newlines
       finally
          (adjust-newline-gap stream newlines)
          (return
            (cond
              ((eql #\newline (skip-c-whitespace stream))
               (read-char stream t nil t)
               +newline-marker+)
              (t
               (values))))))
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
                           :stream stream
                           :format-control "Character '~C' is not a hexadecimal for universal character names."
                           :format-arguments (list char))
               do (write-char char out))))
         (code (parse-integer hexadecimals :radix 16)))
    ;; See 6.4.3 Universal character names in ISO/IEC 9899:1999.
    (when (or (and (< code #x00A0)
                   (not (member code '(#x0024 #x0040 #x0060)))) ; $, @, `
              (<= #xD8000 code #xDFFF))
      (error 'with-c-syntax-reader-error
             :stream stream
             :format-control "Universal character name '~A' is not usable."
             :format-arguments (list hexadecimals)))
    ;; FIXME: This code assumes `code-char' uses unicode code point,
    ;; but it is not standard.  I must use a kind of Unicode library,
    ;; for *correct* implementation.
    (code-char code)))

(defun read-numeric-escape (stream radix &key (limit most-positive-fixnum))
  "Used by `read-escaped-char'."
  ;; C standard says '\xxx' works on execution code set.
  ;; See "6.4.4.4 Character constants", in ISO/IEC 9899:1999, page 61.
  (loop with tmp = 0
        for count from 0 below limit   ; Octal escape is limited for 3 characters.
	for c = (peek-char nil stream nil nil t)
	as weight = (and c (digit-char-p c radix))
	while weight
	do (read-char stream t nil t)
	   (setf tmp (+ (* tmp radix) weight))
	finally (return (code-char tmp))))

(defun read-escaped-char (stream)
  (let ((c0 (read-char stream t nil t)))
    (case c0
      (#\a +bel-character+)             ; alarm
      (#\b #\Backspace)
      (#\f #\Page)
      (#\n #\Newline)
      (#\r #\Return)
      (#\t #\Tab)
      (#\v +vertical-tab-character+)    ; vertical tab
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
              :stream stream
              :format-control "Bad escaped char: ~C."
              :format-arguments (list c0))))))

(defun read-single-quote (stream &optional (c0 #\'))
  (declare (ignore c0))
  (let ((c1 (read-char stream t nil t)))
    (case c1
      (#\'
       (error 'with-c-syntax-reader-error
              :stream stream
	      :format-control "Empty char constant."))
      (#\newline
       (error 'with-c-syntax-reader-error
              :stream stream
	      :format-control "Char constant cannot contain a newline directly."))
      (#\\
       (setf c1 (read-escaped-char stream))))
    (let ((c2 (read-char stream t nil t)))
      (unless (char= c2 #\')
	(error 'with-c-syntax-reader-error
               :stream stream
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
                       :stream stream
	               :format-control "String literal cannot contain a newline directly.")
      else
        do (write-char c out))))

(defun read-L (stream &optional (char #\L))
  (case (peek-char nil stream nil nil t)
    (#\'
     (read-single-quote stream (read-char stream t nil t)))
    (#\"
     (read-double-quote stream (read-char stream t nil t)))
    (otherwise
     (let ((*readtable* (copy-readtable)))
       (set-syntax-from-char #\L #\L)   ; Remove #\L reader macro temporarily.
       (read-after-unread char stream t nil t)))))

(defun read-dot (stream char)
  "Dot may be an '.' operator, '...', or a prefix of floating numbers."
  (assert (char= char #\.))
  (let ((next (peek-char nil stream nil nil t)))
    (cond ((and next
                (digit-char-p next 10))
           (read-numeric-literal stream char))
          ((eql next #\.)
           (read-char stream t nil t)
           (cond ((eql #\. (peek-char nil stream nil nil t))
                  (read-char stream)
                  (intern "..."))
                 (t
                  (setf *second-unread-char* #\.) ; We can't use `unread-char' because `peek-char' was used above.
                  (intern "."))))
          (t
           (read-single-character-symbol stream char)))))

(defun read-single-or-compound-assign (stream char)
  (case (peek-char nil stream nil nil t)
    (#\=
     (read-char stream t nil t)
     (symbolicate char #\=))
    (otherwise
     (read-single-character-symbol stream char))))

(defun read-plus-like-symbol (stream char)
  "For '+', '&', '|'. They may be '+', '++', or '+='"
  (let ((next (peek-char nil stream nil nil t)))
    (cond ((eql next char)
	   (read-char stream t nil t)
	   (symbolicate char next))
	  (t
	   (read-single-or-compound-assign stream char)))))

(defun read-minus (stream char)
  (assert (char= char #\-))
  (case (peek-char nil stream nil nil t)
    (#\>
     (read-char stream t nil t)
     (intern "->"))
    (t
     (read-plus-like-symbol stream char))))

(defun read-shift (stream char)
  "For '<', '>'. They may be '>', '>>', and '>=', '>>='."
  (let ((next (peek-char nil stream nil nil t)))
    (cond ((eql next char)            ; shift op
	   (read-char stream t nil t)
           (case (peek-char nil stream nil nil t)
             (#\=
	      (read-char stream t nil t)
	      (symbolicate char next #\=))
	     (otherwise
	      (symbolicate char next))))
	  (t
	   (read-single-or-compound-assign stream char)))))

(defun read-< (stream char)
  (assert (char= char #\<))
  (let ((next (peek-char nil stream nil nil t)))
    (case next
      ((#\: #\%)                        ; digraph '<:' or '<%'
       (read-char stream t nil t)
       (symbolicate char next))
      (t
       (read-shift stream char)))))

(defun read-slash (stream char)
  (read-slash-comment stream char
                      #'read-single-or-compound-assign))

(defun read-% (stream char)
  (assert (char= char #\%))
  (case (peek-char nil stream nil nil t)
    (#\>                                ; digraph '%>'
     (read-char stream t nil t)
     (intern "%>"))
    (#\:                                ; digraph '%:' or '%:%:'
     (read-char stream t nil t)
     (cond
       ((eql #\% (peek-char nil stream nil nil t))
        (read-char stream t nil t)
        (cond
          ((eql #\: (peek-char nil stream nil nil t))
           (read-char stream t nil t)
           (intern "%:%:"))
          (t
           (setf *second-unread-char* #\%) ; We can't use `unread-char' because `peek-char' was used above.
           (intern "%:"))))
       (t
        (intern "%:"))))
    (t
     (read-single-or-compound-assign stream char))))

(defun read-colon (stream char)
  (assert (char= char #\:))
  (case (peek-char nil stream nil nil t)
    (#\>
     (read-char stream t nil t)
     (intern ":>"))                     ; digraph ':>'
    (t
     (read-single-character-symbol stream char))))

(defun read-sharp (stream char)
  (assert (char= char #\#))
  (case (peek-char nil stream nil nil t)
    (#\#
     (read-char stream t nil t)
     (intern "##"))
    (t
     (read-single-character-symbol stream char))))

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
                             :stream stream
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
              ; FIXME: :stream stream
              :format-control "~A contains an unknown prefix as a numeric constant."
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
                         ; FIXME: :stream stream
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
                  ; FIXME: :stream stream
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
            ; FIXME: :stream stream
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
            ; FIXME: :stream stream
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

(defun read-0x-numeric-literal (stream c0)
  "Read hexadecimal numeric literal of C."
  ;; TODO: merge with `read-lonely-single-symbol'
  (let ((next (peek-char nil stream nil nil t)))
    (if (and next
             (char-equal next #\x))
        (read-numeric-literal stream c0)
        (let ((*readtable* (copy-readtable nil))) ; Use the standard syntax.
          (read-after-unread c0 stream t nil t)))))

(defreadtable c-reader-level-0
  (:merge :standard)
  (:macro-char #\} #'read-right-curly-bracket t)
  ;; Vertical tab is a whitespace in C. (Allegro CL 10.0 is already so).
  (:syntax-from :standard #\space +vertical-tab-character+)
  ;; Comma is read as a symbol.
  (:macro-char #\, #'read-single-character-symbol)
  ;; Enables solely ':' as a symbol.
  (:macro-char #\: #'read-lonely-single-symbol t))

(defreadtable c-reader-level-1
  (:merge c-reader-level-0)
  ;; Treats '0x' numeric literal specially.
  (:macro-char #\0 #'read-0x-numeric-literal t)
  ;; Reads '||' and solely '|' as a symbol.
  ;; FIXME: Should I use Lisp escape for a symbol like '|assert|'?
  (:macro-char #\| #'read-solely-bar t)
  ;; brackets
  (:macro-char #\{ #'read-single-character-symbol)
  (:macro-char #\} #'read-right-curly-bracket)
  (:macro-char #\[ #'read-single-character-symbol)
  (:macro-char #\] #'read-single-character-symbol)
  ;; for accessing normal syntax.
  (:macro-char #\` #'read-in-previous-syntax)
  ;; Disables 'consing dots', with replacement of ()
  (:macro-char #\. #'read-lonely-single-symbol t)
  ;; Enable C comments.
  (:macro-char #\/ #'read-slash-comment t)
  ;; Destroying CL standard syntax -- overwrite standard macro chars.
  (:macro-char #\" #'read-double-quote)
  (:macro-char #\; #'read-single-character-symbol)
  (:macro-char #\( #'read-single-character-symbol)
  (:macro-char #\) #'read-single-character-symbol))
;;; TODO: Add #d(efine), #e(rror|lif|lse|ndif), #i(nclude|f|fdef|fndef), #l(ine), #u(ndef)
;;;    #p(ragma) is conflict with the pathname syntax.

(defreadtable c-reader-level-2
  (:fuse c-reader-level-1)
  ;; Character constant, overwrites `quote'.
  ;; (The overwriting prevents Lisp syntax extremely, so enables only in level 2).
  (:macro-char #\' #'read-single-quote)
  ;; 'L' prefix for character and string.
  (:macro-char #\L #'read-L t)
  ;; C punctuators.
  ;;   Already in Level 0 -- ,
  ;;   Already in Level 1 -- [ ] { } ( ) ;
  (:macro-char #\. #'read-dot)                       ; . ...
  (:macro-char #\- #'read-minus)                     ; -> -- - -=
  (:macro-char #\+ #'read-plus-like-symbol)          ; ++ + +=
  (:macro-char #\& #'read-plus-like-symbol)          ; & && &=
  (:macro-char #\* #'read-single-or-compound-assign) ; * *=
  (:macro-char #\~ #'read-single-character-symbol)   ; ~
  (:macro-char #\! #'read-single-or-compound-assign) ; ! !=
  (:macro-char #\/ #'read-slash)        ; / /= and comments.
  (:macro-char #\% #'read-%)            ; % %= %> %: %:%:
  (:macro-char #\< #'read-<)            ; << < <= <<= <: <%
  (:macro-char #\> #'read-shift)        ; >> > >= >>=
  (:macro-char #\= #'read-single-or-compound-assign) ; == =
  (:macro-char #\^ #'read-single-or-compound-assign) ; ^ ^=
  (:macro-char #\| #'read-plus-like-symbol)          ; | || |=
  (:macro-char #\? #'read-single-character-symbol)   ; ?
  (:macro-char #\: #'read-colon)                     ; : :>
  (:macro-char #\# #'read-sharp)                     ; # ##
  ;; Numeric litrals.
  (:macro-char #\0 #'read-numeric-literal t)
  (:macro-char #\1 #'read-numeric-literal t)
  (:macro-char #\2 #'read-numeric-literal t)
  (:macro-char #\3 #'read-numeric-literal t)
  (:macro-char #\4 #'read-numeric-literal t)
  (:macro-char #\5 #'read-numeric-literal t)
  (:macro-char #\6 #'read-numeric-literal t)
  (:macro-char #\7 #'read-numeric-literal t)
  (:macro-char #\8 #'read-numeric-literal t)
  (:macro-char #\9 #'read-numeric-literal t)
  ;; TODO: C99 support?
  ;; - An identifier begins with '\u' (universal character)
  ;;   How to be '\' treated?
  )

(defun make-c-readtable (level readtable-case)
  "Returns a readtable for tokenize C source. See `*with-c-syntax-reader-level*'."
  (let* ((c-readtable-name
           (ecase level
             (0 'c-reader-level-0)
             (1 'c-reader-level-1)
             (2 'c-reader-level-2)))
         (c-readtable (find-readtable c-readtable-name)))
    (if readtable-case
        (let ((new-readtable (copy-readtable c-readtable)))
          (setf (readtable-case new-readtable) readtable-case)
          new-readtable)
        c-readtable)))

(defconstant +newline-marker+
  '+newline-marker+
  "Used for saving newline chars from reader to preprocessor.")

(defconstant +whitespace-marker+
  '+whitespace-marker+
  "Used for saving whitespace chars from reader to preprocessor, for preprocessor directives.")

(defun skip-c-whitespace (stream)
  "Skips C whitespaces except newline."
  (loop for c = (read-char stream)
        while (and (not (eql #\newline c))
                   (c-whitespace-p c))
        count c into cnt
        finally
           (unread-char c stream)
           (return (values c cnt))))

(defun read-preprocessing-token (stream c-readtable keep-whitespace recursive-p)
  "Reads a token from STREAM until EOF or '}#' found. Newline is read
 as `+newline-marker+'."
  (let ((*readtable* c-readtable))
    (cond
      (*second-unread-char*
       (assert (not (c-whitespace-p *second-unread-char*)))
       (read-after-unread (shiftf *second-unread-char* nil) stream t nil recursive-p))
      (t
       (multiple-value-bind (first-char whitespaces)
           (skip-c-whitespace stream)
         (cond
           ((and keep-whitespace (plusp whitespaces))
            +whitespace-marker+)
           ((eql first-char #\newline)
            ;; Preserve newline to preprocessor.
            (read-char stream)
            +newline-marker+)
           ((eql first-char #\})
            ;; This path is required for SBCL.
            ;; On Allegro, it is not needed. '}' reader macro (`read-right-curly-bracket') works good.
            (read-char stream)
            (cond ((eql (peek-char nil stream t) #\#)
                   (read-char stream)
                   +wcs-end-marker+)
                  (t
                   (intern "}")))) ; Intern it into the current `*package*'.
           (t
            (read-preserving-whitespace stream t :eof recursive-p))))))))

(defun tokenize-source (level stream reader-macro-mode)
  "Tokenize C source by doing translation phase 1, 2, and 3.
 LEVEL is the reader level described in `*with-c-syntax-reader-level*'"
  (let* ((*read-default-float-format* 'double-float) ; In C, floating literal w/o suffix is double.
         (*previous-syntax* *readtable*)
         (*second-unread-char* nil)
         (c-readtable (make-c-readtable level *with-c-syntax-reader-case*))
         (process-backslash-newline
           (case *with-c-syntax-reader-process-backslash-newline*
             (:auto (>= level +with-c-syntax-default-reader-level+))
             (otherwise *with-c-syntax-reader-process-backslash-newline*))))
    (loop
      with cp-stream = (make-instance 'physical-source-input-stream
                                      :stream stream :target-readtable c-readtable
                                      :phase-2 process-backslash-newline)
      with keep-whitespace = nil
      
      for token = (handler-case
                      (read-preprocessing-token cp-stream c-readtable keep-whitespace
                                                reader-macro-mode)
                    (end-of-file (e)
                      (if reader-macro-mode
                          (error e)
                          (loop-finish))))
      do (cond
           ((eq token +wcs-end-marker+)
            (loop-finish))
           ((and (symbolp token)
                 (or (string= token "#") (string= token "%:")))
            (setf keep-whitespace t))
           ((eq token +newline-marker+)
            (setf keep-whitespace nil)))
      collect token into token-list
      finally
         (return (values token-list c-readtable)))))

(defun read-in-c-syntax (stream char n)
  "Called by '#{' reader macro of `with-c-syntax-readtable'.
Inside '#{' and '}#', the reader uses completely different syntax, and
the result is wrapped with `with-c-syntax'.
 See `*with-c-syntax-reader-level*' and `*with-c-syntax-reader-case*'."
  (assert (char= char #\{))
  (let ((level (alexandria:clamp (or n *with-c-syntax-reader-level*) 0 2))
        (input-file-pathname (ignore-errors (namestring stream))))
    (multiple-value-bind (tokens readtable)
        (tokenize-source level stream t)
      ;; TODO: Move these parameters to #pragma?
      `(with-c-syntax (:readtable-case
                       ;; Capture the readtable-case used for reading inside '#{ ... }#'.
                       ,(readtable-case readtable)
                       :input-file-pathname ,input-file-pathname)
         ,@tokens))))

(defreadtable with-c-syntax-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'read-in-c-syntax))

;;; Sadly, `defreadtable' does not have docstring syntax..
;;; So, I added them into `read-in-c-syntax'.

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
