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
- '.' :: '.'
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

(eval-when (:compile-toplevel)
  (defconstant +acceptable-numeric-characters+
    '(( 2 . (#\0 #\1))
      ( 8 . (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
      (10 . (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.))
      (16 . (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))))

(defun read-bare-number (&optional (base 10) (stream *standard-input*) (c0 nil))
  (let ((string (make-array '(0) :element-type 'character :adjustable t))
        (floatp)
        (acceptable-characters (cdr (assoc base +acceptable-numeric-characters+))))
    (when c0
      (vector-push-extend c0 string))
    (loop for c = (peek-char nil stream)
          while (cond
                  ((char= c #\') (read-char stream)) ; Skip character
                  ((char= c #\.) (setf floatp t) (vector-push-extend (read-char stream) string))
                  ((member c acceptable-characters) (vector-push-extend (read-char stream) string))
                  (t nil))) ; Finish processing if any other character
    (if floatp
        (let ((*readtable* named-readtables::*standard-readtable*))
          (read-from-string string))
        (parse-integer string :radix base))))

(defun read-numeric-literal (stream c0)
  "Read a C numeric literal of approximate format (0([XxBb])?)?([0-9A-Fa-f.']+)([Uu])?([Ll]{1,2})? .
Scientific notation not yet supported."
  (let* ((c1 (peek-char nil stream))
         ;; 1. Determine base
         (base (if (char= c0 #\0)
                   (cond
                     ((char-equal c1 #\b) (read-char stream) 2)
                     ((char-equal c1 #\x) (read-char stream) 16)
                     ((member c1 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)) 8)
                     (t 10)) ; c1 is probably #\u or #\l or part of next token
                   10))
         ;; 2. Read number, including fractional part
         (number (if (= base 10)
                     (read-bare-number base stream c0)
                     (read-bare-number base stream)))
         (signedp)
         (longp)
         (longlongp))
    (tagbody
       (macrolet
           ((read-signifier (variable signifier expected-chars)
              `(let ((char (peek-char nil stream)))
                 (cond
                   ((char-equal char ,signifier) (read-char stream) (setf ,variable t))
                   ((not (member char ,expected-chars)) (go finish-processing))))))
         ;; 3. Determine signedness
         (read-signifier signedp #\u '(#\U #\u #\L #\l))
         ;; 4. Determine size
         (read-signifier longp #\l '(#\L #\l))
         (read-signifier longlongp #\l '(#\L #\l)))
       ;; 5. Construct CL value of appropriate type and return it
     finish-processing
       ;; For now, signedp/longp/longlongp are ignored.
     (return-from read-numeric-literal number))))

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
    (set-macro-character #\. #'read-single-character-symbol nil readtable)
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
