(in-package #:with-c-syntax.core)

(defconstant +with-c-syntax-default-reader-level+ 1)

(defvar *with-c-syntax-reader-level* nil
  "Holds the reader level used by '#{' reader function.
The value is one of 0, 1, 2, or nil (default).
The default is nil, recognized same as `+with-c-syntax-default-reader-level+'.

For inside '#{' and '}#', three syntaxes are defined. These syntaxes
are selected by the infix parameter of the '#{' dispatching macro
character. If it not specified, its default is this value.

Available syntaxes are below:


* Level 0 (conservative)

In level 0, these reader macros are installed.

- ',' :: ',' is read as a symbol. (In ANSI CL, a comma is defined as
         an invalid char outside the backquote syntax.)
- ':' :: Reads a solely ':' as a symbol. Not a solely one (as a
         package marker) works as is.

Level 0 is almost comatible with the standard syntax. However, we need
many escapes using C symbols.


* Level 1 (aggressive).

In level 1, these reader macros are installed.

- '{', '}', '[', ']' :: These become a terminating character,
                        and read as a symbol.
- '`' :: '`' reads a next s-exp in the previous syntax. This works as
         an escape from '#{' and '}#' The 'backquote' functionality is
         lost.
- '.' :: Reads a solely '.' as a symbol. The 'consing dot'
         functionality is lost.
- '\' :: The '\' becomes a ordinary constituent character. The
         'multiple escaping' functionality is lost.
- '/' :: '//' means a line comment, '/* ... */' means a block comment.
         '/' is still non-terminating, and has special meanings only
         if followed by '/' or '*'. Ex: 'a/b/c' or '/+aaa+/' are still
         valid symbols.
- ''' (single-quote) :: The single-quote works as a character literal
                        of C. The 'quote' functionality is lost.
- '\"' (double-quote) :: The double-quote works as a string literal of
                         C. Especially, escaping is treated as C. The
                         original functionality is lost.
- ';' :: ';' becomes a terminating character, and read as a symbol.
         The 'comment' functionality is lost.
- '(' and ')' :: parenthesis become a terminating character, and read
                 as a symbol.  The 'reading a list' functionality is
                 lost.

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
- '.' :: '.' or a numeric literal of C language..
- 0,1,2,3,4,5,6,7,8,9 :: A numeric literal of C language.

In this level, there is no compatibilities between symbols of Common
Lisp.  Especially, for denoting a symbol has terminating characters,
escapes are required. (ex. most\-positive\-fixnum)")

(defvar *with-c-syntax-reader-case* nil
  "Holds the reader case used by '#{' reader function.

When this is not nil, it must be one of `:upcase', `:downcase',
`:preserve', or `:invert'.  The specified case is used as the
readtable-case inside '#{' and '}#' and passed to the
wrapping `with-c-syntax' form.

When this is nil, it is used as the readtable-case of `*readtable*' at
'#{'.")

(defvar *previous-syntax* (copy-readtable)
  "Holds the readtable used by #\` syntax.
This is bound by '#{' read macro to the `*readtable*' at that time.")


(defun read-in-previous-syntax (stream char)
  (declare (ignore char))
  (let ((*readtable* *previous-syntax*))
    (read stream t nil t)))

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (symbolicate char))

(defun read-lonely-single-symbol (stream char)
  (loop with buf = (make-array '(1) :element-type 'character
			       :initial-contents `(,char)
			       :adjustable t :fill-pointer t)
     for c = (peek-char nil stream t nil t)
     until (terminating-char-p c)
     do (read-char stream t nil t)
       (vector-push-extend c buf)
     finally
       (if (length= 1 buf)
	   (return (symbolicate buf))
	   (let ((*readtable* (copy-readtable)))
	     (set-syntax-from-char char #\@) ; gets the constituent syntax.
	     (return (read-from-string buf t nil))))))

(defun read-2chars-delimited-list (c1 c2 &optional stream recursive-p)
  (loop for lis = (read-delimited-list c1 stream recursive-p)
     nconc lis
     until (char= c2 (peek-char nil stream t nil recursive-p))
     collect (symbolicate c1)	; assumes c1 is a kind of terminating.
     finally
       (assert (char= c2 (read-char stream t nil recursive-p))))) ; eat the peeked char.

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

(defun read-single-or-equal-symbol (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (case next
      (#\=
       (read-char stream t nil t)
       (symbolicate char next))
      (otherwise
       (read-single-character-symbol stream char)))))

(defun read-single-or-equal-or-self-symbol (stream char)
  "For '>>=' or '<<='."
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next char)
	   (read-char stream t nil t)
	   (symbolicate char next))
	  (t
	   (read-single-or-equal-symbol stream char)))))

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
	   (read-single-or-equal-symbol stream char)))))

(defun read-slash (stream char)
  (read-slash-comment stream char
                      #'read-single-or-equal-symbol))

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
  (multiple-value-bind (base floatp)
      ;; See prefix.
      (case (char pp-number-string 0)   ; I use `ecase' 
        (#\.
         (values 10 t)) ; fractional-constant, a part of decimal-floating-constant.
        (#\0
         (if (length= 1 pp-number-string) ; '0'
             (values 10 nil)
             (case (char pp-number-string 1)
               ((#\b #\B)               ; binary-constant. Since C23.
                (values 2 nil))
               ((#\x #\X) ; hexadecimal-constant or hexadecimal-floating-constant.
                (values 16 :unspecific))
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) ; octal-constant.
                (values 8 nil))
               (otherwise
                (values 10 :unspecific))))) ; Decimal. May be '0u' or '0.'
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ; decimal-constant or decimal-floating-constant
         (values 10 :unspecific))
        (otherwise     ; This is a bug of `read-preprocessing-number'.
         (error 'with-c-syntax-reader-error
                :format-control "~A contains a prefix as a numeric constants."
                :format-arguments (list pp-number-string))))
    ;; Determine type.
    (when (eq floatp :unspecific)
      (setf floatp
            (find-if (ecase base
                       (10 (lambda (c) (member c '(#\. #\e #\E))))
                       (16 (lambda (c) (member c '(#\. #\p #\P))))) 
                     pp-number-string)))
    (values base floatp)))

(defun number-prefix-length (radix)
  (ecase radix
    (10 0)
    (8 1)
    ((2 16) 2)))

(defun read-integer-constant (pp-number-string radix)
  (let ((string (make-array (length pp-number-string)
                            :element-type 'character :fill-pointer 0))
        (integer-suffix-exists nil)
        (unsigned-p nil)
        (integer-size nil))
    (with-input-from-string (stream pp-number-string :start (number-prefix-length radix))
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
                         :format-arguments (list pp-number-string c radix)))))
      (when integer-suffix-exists
        (loop
          for suffix1 = (read-char stream nil nil)
          while suffix1
          do (case suffix1
               ((#\u #\U)
                (when unsigned-p
                  (error 'with-c-syntax-reader-error
                         :format-control "Integer constant '~A' contains two unsigned-suffix (radix ~D)."
                         :format-arguments (list pp-number-string radix)))
                (setf unsigned-p t))
               ((#\l #\L)
                (when integer-size
                  (error 'with-c-syntax-reader-error
                         :format-control "Integer constant '~A' contains two long-suffix (radix ~D)."
                         :format-arguments (list pp-number-string radix)))
                (let ((suffix2 (peek-char nil stream nil nil)))
                  (case suffix2
                    ((#\l #\L)
                     (unless (char= suffix1 suffix2)
                       (error 'with-c-syntax-reader-error
                              :format-control "Integer constant '~A' contains invalid suffix '~C~C' (radix ~D)."
                              :format-arguments (list pp-number-string suffix1 suffix2 radix)))
                     (read-char stream t nil t)
                     (setf integer-size 'long-long))
                    (otherwise
                     (setf integer-size 'long)))))
               (otherwise
                (error 'with-c-syntax-reader-error
                       :format-control "Integer constant '~A' contains invalid suffix '~C' (radix ~D)."
                       :format-arguments (list pp-number-string suffix1 radix)))))))
    (values
     (parse-integer string :radix radix)
     unsigned-p
     integer-size)))

(defun read-numeric-literal (stream c0)
  "Read a C numeric literal of approximate format (0([XxBb])?)?([0-9A-Fa-f.']+)([Uu])?([Ll]{1,2})? .
Scientific notation not yet supported."
  (let ((pp-number (read-preprocessing-number stream c0)))
    (multiple-value-bind (radix floatp)
        (find-numeric-literal-type pp-number)
      (if floatp
          (error "TODO")
          (read-integer-constant pp-number radix)))))

(defun install-c-reader (readtable level)
  "Inserts reader macros for C reader. Called by '#{' reader macro."
  (check-type level integer)
  (when (>= level 0)			; Conservative
    ;; Comma is read as a symbol.
    (set-macro-character #\, #'read-single-character-symbol nil readtable)
    ;; Enables solely ':' as a symbol.
    (set-macro-character #\: #'read-lonely-single-symbol t readtable))
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
    ;; removes 'multi-escape'
    (set-syntax-from-char #\| #\& readtable)
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
    (set-macro-character #\= #'read-single-or-equal-symbol nil readtable)
    (set-macro-character #\* #'read-single-or-equal-symbol nil readtable)
    (set-macro-character #\% #'read-single-or-equal-symbol nil readtable)
    (set-macro-character #\^ #'read-single-or-equal-symbol nil readtable)
    (set-macro-character #\! #'read-single-or-equal-symbol nil readtable)
    (set-macro-character #\& #'read-single-or-equal-or-self-symbol nil readtable)
    (set-macro-character #\| #'read-single-or-equal-or-self-symbol nil readtable)
    (set-macro-character #\+ #'read-single-or-equal-or-self-symbol nil readtable)
    (set-macro-character #\- #'read-minus nil readtable)
    (set-macro-character #\< #'read-shift nil readtable)
    (set-macro-character #\> #'read-shift nil readtable)
    (set-macro-character #\/ #'read-slash nil readtable)
    ;; (set-macro-character #\- #'read-numeric-literal nil readtable) ;; FIXME: Handle negative numbers correctly
    (set-macro-character #\0 #'read-numeric-literal nil readtable)
    (set-macro-character #\1 #'read-numeric-literal nil readtable)
    (set-macro-character #\2 #'read-numeric-literal nil readtable)
    (set-macro-character #\3 #'read-numeric-literal nil readtable)
    (set-macro-character #\4 #'read-numeric-literal nil readtable)
    (set-macro-character #\5 #'read-numeric-literal nil readtable)
    (set-macro-character #\6 #'read-numeric-literal nil readtable)
    (set-macro-character #\7 #'read-numeric-literal nil readtable)
    (set-macro-character #\8 #'read-numeric-literal nil readtable)
    (set-macro-character #\9 #'read-numeric-literal nil readtable))
  ;; TODO: If I support trigraphs or digraphs, I'll add them here.
  ;; (But I think these are not needed because the Standard characters include
  ;; the replaced characters/)
  ;; TODO: C99 support?
  ;; - an identifier begins with '\u' (universal character)
  ;; - 'L' prefix of character literals.
  readtable)

(defun read-in-c-syntax (stream char n)
  "Called by '#{' reader macro of `with-c-syntax-readtable'.
Inside '#{' and '}#', the reader uses completely different syntax, and
wrapped with `with-c-syntax' form.
 See `*with-c-syntax-reader-level*' and `*with-c-syntax-reader-case*'."
  (assert (char= char #\{))
  (let* ((*previous-syntax* *readtable*)
         (*readtable* (copy-readtable))
         (*read-default-float-format* 'double-float) ; In C, floating literal w/o suffix is double.
         (level (if n (alexandria:clamp n 0 2)
                    (or *with-c-syntax-reader-level*
                        +with-c-syntax-default-reader-level+)))
         (keyword-case (or *with-c-syntax-reader-case*
                           (readtable-case *readtable*))))
    (setf (readtable-case *readtable*) keyword-case)
    (install-c-reader *readtable* level)
    ;; I forgot why this is required.. (2018-11-12)
    ;; (set-dispatch-macro-character #\# #\{ #'read-in-c-syntax)
    `(with-c-syntax (:keyword-case ,keyword-case)
       ,@(read-2chars-delimited-list #\} #\# stream t))))

(defreadtable with-c-syntax-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'read-in-c-syntax))

;;; Sadly, `defreadtable' does not have docstring syntax..
;;; So, I added them into `read-in-c-syntax'.

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
