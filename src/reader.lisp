(in-package #:with-c-syntax.core)

(define-constant +bracket-pair-alist+
    '((#\( . #\)) (#\[ . #\])
      (#\{ . #\}) (#\< . #\>))
  :test 'equal
  :documentation
  "Holds an alist of bracket pairs.")

(defconstant +with-c-syntax-default-reader-level+ 1)

(defvar *with-c-syntax-reader-level* nil
  "Holds the reader level used by '#{' reader function.

The value is one of 0, 1, 2, or nil (default).
The default is nil, recognized same as `+with-c-syntax-default-reader-level+'.

For inside '#{' and '}#', four syntaxes are defined. These syntaxes
are selected by the infix parameter of the '#{' dispatching macro
character. If it not specified, its default is this value.

If you interest for what syntaxes are defined, Please see the 'reader.lisp'")
;;; TODO: move docstrings of `use-reader'.

(defvar *with-c-syntax-reader-case* nil
  "* Value Type
a symbol or nil

* Description
Holds the reader case used by '#{' reader function.

When this is not nil, the specified case is used as the
readtable-case inside '#{' and '}#', and the case is passed to the
wrapping ~with-c-syntax~ form.

When this is nil, the readtable-case of ~*readtable*~ at using
'#{' is used.
")

(defvar *previous-syntax* nil
  "* Value Type
a readtable.

* Description
Holds the readtable used by #\` syntax.
Default is nil, which is treated as the standard readtable.")


(defun read-in-previous-syntax (stream char)
  (declare (ignore char))
  (let ((*readtable* (or *previous-syntax* (copy-readtable nil))))
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

(defun read-escaped-char (stream)
  (flet ((numeric-escape (radix init)
	   (loop with tmp = init
	      for c = (peek-char nil stream t nil t)
	      as weight = (digit-char-p c radix)
	      while weight
	      do (read-char stream t nil t)
		(setf tmp (+ (* tmp radix) weight))
	      finally (return (code-char tmp)))))
    (let ((c0 (read-char stream t nil t)))
      (case c0
        ;; FIXME: This code assumes ASCII is used for char-code..
	(#\a #.(code-char #x07))	; alarm
	(#\b #\Backspace)
	(#\f #\Page)
	(#\n #\Newline)
	(#\r #\Return)
	(#\t #\Tab)
	(#\v #.(code-char #x0b))	; vertical tab
	(#\\ #\\)
	(#\' #\')
	(#\" #\")
	(#\? #\?)
	((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
	 (numeric-escape 10 (digit-char-p c0)))
	(#\x
	 (numeric-escape 16 0))
        (otherwise
         (error 'with-c-syntax-reader-error
                :format-control "Bad escaped char: ~C."
                :format-arguments (list c0)))))))

(defun read-single-quote (stream c0)
  (let ((c1 (read-char stream t nil t)))
    (cond ((char= c1 c0)
	   (error 'with-c-syntax-reader-error
		  :format-control "Empty char constant."))
	  ((char= c1 #\\)
	   (setf c1 (read-escaped-char stream))))
    (let ((c2 (read-char stream t nil t)))
      (unless (char= c2 c0)
	(error 'with-c-syntax-reader-error
               :format-control "Too many chars appeared between '' :~C, ~C, ..."
               :format-arguments (list c1 c2))))
    c1))

(defun read-double-quote (stream c0)
  (loop with str = (make-array '(0) :element-type 'character
			       :adjustable t :fill-pointer t)
     for c = (read-char stream t nil t)
     until (char= c c0)
     do (vector-push-extend
         (if (char= c #\\) (read-escaped-char stream) c)
         str)
     finally (return str)))

(defun read-single-or-equal-symbol (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (case next
      (#\=
       (read-char stream t nil t)
       (symbolicate char next))
      (otherwise
       (read-single-character-symbol stream char)))))

(defun read-single-or-equal-or-self-symbol (stream char)  
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
    (set-macro-character #\` #'read-in-previous-syntax readtable)
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
    (set-macro-character #\/ #'read-slash nil readtable))
  readtable)

(defun read-in-c-syntax (stream char n)
  (let* ((*readtable* (copy-readtable))
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
       ,@(read-2chars-delimited-list
	  (cdr (assoc char +bracket-pair-alist+))
	  #\# stream t))))


(defreadtable with-c-syntax-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'read-in-c-syntax))

;;; Sadly, `defreadtable' does not have docstring syntax..
#|
This readtable supplies '#{' reader macro.
Inside '#{' and '}#', the reader uses completely different syntax, and
wrapped with ~with-c-syntax~ form.
|#


;;; Old hand-made syntax changing macro. (TODO: move these docstrings).

(defmacro use-reader (&key level case)
  "* Syntax
~use-reader~ &key level case => readtable

* Arguments and Values
- level :: one of 0, 1, or 2.
           The default is specified by `*with-c-syntax-reader-level*'.
- case :: one of ~:upcase~, ~:downcase~, ~:preserve~, ~:invert~, or
          nil. The default is nil.

* Description
This macro establishes a C syntax reader.

`use-reader' introduces a dispatching macro character '#{'.  Inside
'#{' and '}#', the reader uses completely different syntax, and
wrapped with `with-c-syntax' form.

** Syntax Levels
For inside '#{' and '}#', four syntaxes are defined. These syntaxes
are selected by the infix parameter of the '#{' dispatching macro
character. If it not specified, The default is the LEVEL specified
at `use-reader;.

*** Level 0 (conservative)

In this level, these reader macros are installed.

- ',' :: ',' is read as a symbol. (In ANSI CL, a comma is defined as
         an invalid char outside the backquote syntax.)
- ':' :: Reads a solely ':' as a symbol. Not a solely one (as a
         package marker) works as is.

*** Level 1 (aggressive)

In this level, these reader macros are installed.

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

In this level, '(' and ')' loses its functionalities. For constructing
a list, the '`' syntax must be used.

*** Level 2 (overkill)

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
escapes are required. (ex. most\-positive\-fixnum)

** Syntax Cases
When ~case~ is not nil, the specified case is used as the
readtable-case inside '#{' and '}#', and the case is passed to the
wrapping ~with-c-syntax~ form.

When ~case~ is nil, the readtable-case of ~*readtable*~ at using
'#{' is used.

* Side Effects
Changes ~*readtable*~.

* Notes
There is no support for trigraphs or digraphs.

* See Also
~with-c-syntax~, ~unuse-reader~.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; (2018-11-13) In old code, I stucked readtables into a list.
     ;; But I removed this feature because I use named-readtables.
     (shiftf *previous-syntax* *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\{ #'read-in-c-syntax)
     (setf *with-c-syntax-reader-level* ,level
	   *with-c-syntax-reader-case* ,case)
     *readtable*))

(defmacro unuse-reader ()
  "* Syntax
~unuse-reader~ <no arguments> => readtable

* Arguments and Values
- readtable :: a readtable

* Description
Disposes the C reader established by ~use-reader~, and restores the
previous readtable.

* Side Effects
Changes ~*readtable*~.

* See Also
~unuse-reader~.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *with-c-syntax-reader-level* nil
	   *with-c-syntax-reader-case* nil)
     (shiftf *readtable* *previous-syntax*)))

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
