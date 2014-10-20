(in-package #:with-c-syntax.core)

(define-constant +reader-level-specifier-alist+
    '((0 . 0) (1 . 1) (2 . 2) (3 . 3)
      (:conservative . 0) (:aggressive . 1)
      (:overkill . 2) (:insane . 3))
  :test 'equal
  :documentation
  "* Value Type
alist :: <atom> -> <fixnum>.

* Description
This constant holds an alist translates 'reader level'.
See ~use-reader~.
")

(defun translate-reader-level (rlspec)
  (cdr (assoc rlspec +reader-level-specifier-alist+
	      :test #'eq)))

(defvar *current-c-reader* nil
  "* Value Type
a list :: consists of plists.

* Description
This variable holds the current c-syntax reader environments,
establised by the ~use-reader~.

Its contents is a list of plists. The plists holds below:
- :level    -> the specified reader level.
- :case     -> the specified reader case.
- :previous -> the readtable used when ~use-reader~ called.
")

(defun read-in-previous-syntax (stream char &optional n)
  (declare (ignore char n))
  (let ((*readtable*
	 (getf (first *current-c-reader*) :previous)))
    (read stream t nil t)))

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (intern (string char)))

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
	   (return (intern buf))
	   (let ((*readtable* (copy-readtable)))
	     (set-syntax-from-char char #\@) ; gets the constituent syntax.
	     (return (read-from-string buf t nil))))))

(defun read-2chars-delimited-list (c1 c2 &optional stream recursive-p)
  (loop for lis = (read-delimited-list c1 stream recursive-p)
     as next = (peek-char nil stream t nil recursive-p)
     nconc lis
     when (char= next c2)
     do (read-char stream t nil recursive-p) (loop-finish)
     else
     collect (intern (string c1))))	; assumes c1 is terminating.

(defun read-single-quote (stream c0)
  (let ((c1 (read-char stream t nil t)))
    (when (char= c1 c0)
      (error "Empty char constant"))
    (let ((c2 (read-char stream t nil t)))
      (unless (char= c2 c0)
	(error "Too many chars appeared between '' :~C, ~C, ..."
	       c1 c2))
      c1)))

;; XXX: This code assumes ASCII is used for char-code..
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
      (ecase c0
	(#\a (code-char #x07))		; alarm
	(#\b #\Backspace)
	(#\f #\Page)
	(#\n #\Newline)
	(#\r #\Return)
	(#\t #\Tab)
	(#\v (code-char #x0b))		; vartical tab
	(#\\ #\\)
	(#\' #\')
	(#\" #\")
	(#\? #\?)
	((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
	 (numeric-escape 10 (digit-char-p c0)))
	(#\x
	 (numeric-escape 16 0))))))

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
       (intern (make-string-from-chars char next)))
      (t
       (read-single-character-symbol stream char)))))

(defun read-single-or-equal-or-self-symbol (stream char)  
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next char)
	   (read-char stream t nil t)
	   (intern (make-string-from-chars char next)))
	  (t
	   (read-single-or-equal-symbol stream char)))))

(defun read-minus (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (cond ((char= next #\>)
	   (read-char stream t nil t)
	   (intern (make-string-from-chars char next)))
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
		(intern (make-string-from-chars char next next2)))
	       (t
		(intern (make-string-from-chars char next))))))
	  (t
	   (read-single-or-equal-symbol stream char)))))

(defun read-slash (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (case next
      (#\/
       (read-line stream t nil t)
       (values))
      (#\*
       (read-char stream t nil t)
       (loop for after-* = (progn (peek-char #\* stream t nil t)
                                  (read-char stream t nil t)
                                  (read-char stream t nil t))
          until (char= after-* #\/))
       (values))
      (t
       (read-single-or-equal-symbol stream char)))))

(defun install-c-reader (readtable level)
  (let ((lev (translate-reader-level level)))
    (unless lev
      (error "Level ~S cannot be accepted" level))
    (setf level lev))
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
    (set-macro-character #\] #'read-single-character-symbol nil readtable))
  (when (>= level 2)			; Overkill
    ;; accessing normal syntax
    (set-macro-character #\` #'read-in-previous-syntax readtable)
    ;; Disables 'consing dots', with replacement of ()
    (set-macro-character #\. #'read-lonely-single-symbol t readtable)
    ;; removes 'multi-escape'
    (set-syntax-from-char #\| #\& readtable)
    ;; destroys CL syntax COMPLETELY!
    (set-macro-character #\' #'read-single-quote nil readtable)
    (set-macro-character #\" #'read-double-quote nil readtable)
    (set-macro-character #\; #'read-single-character-symbol nil readtable)
    (set-macro-character #\( #'read-single-character-symbol nil readtable)
    (set-macro-character #\) #'read-single-character-symbol nil readtable))
  (when (>= level 3)			; Insane
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

(defun read-toplevel-in-c-syntax (stream char n)
  (destructuring-bind (&key level case &allow-other-keys)
      (first *current-c-reader*)
    (let* ((*readtable* (copy-readtable))
	   (keyword-case (or case (readtable-case *readtable*))))
      (setf (readtable-case *readtable*) keyword-case)
      (set-dispatch-macro-character #\# #\{ #'read-toplevel-in-c-syntax)
      (install-c-reader *readtable* (or n level))
      `(with-c-syntax (:keyword-case ,keyword-case)
	 ,@(read-2chars-delimited-list
	    (cdr (assoc char +bracket-pair-alist+ :test #'eq))
	    #\# stream t)))))

(defmacro use-reader (&key (level :overkill) case)
  "* Syntax
~use-reader~ &key level case => readtable

* Arguments and Values
- level :: one of 0, 1, 2, 3, ~:conservative~, ~:aggressive~,
           ~:overkill~, or ~:insane~. The default is ~:overkill~.
- case :: one of ~:upcase~, ~:downcase~, ~:preserve~, ~:invert~, or
          nil. The default is nil.

* Description
This macro establishes a C syntax reader.

~use-reader~ introduces a dispatching macro character '#{'.  Inside
'#{' and '}#', the reader uses completely different syntax, and
wrapped with ~with-c-syntax~ form.

** Syntax Levels
For inside '#{' and '}#', four syntaxes are defined. These syntaxes
are selected by the infix parameter of the '#{' dispatching macro
character. If it not specified, The default is the ~level~ specified
at ~use-reader~.

*** Level 0 (conservative)
This is used when ~level~ is 0 or ~:conservative~.

In this level, these reader macros are installed.

- ',' :: ',' is read as a symbol. (In ANSI CL, a comma is defined as
         an invalid char outside the backquote syntax.)
- ':' :: Reads a solely ':' as a symbol. Not a solely one (as a
         package marker) works as is.

*** Level 1 (aggressive)
This is used when ~level~ is 1 or ~:aggressive~.

In this level, these reader macros are installed.

- '{', '}', '[', ']' :: These become a terminating character,
                        and read as a symbol.

*** Level 2 (overkill)
This is used when ~level~ is 2 or ~:overkill~.

In this level, these reader macros are installed.

- '`' :: '`' reads a next s-exp in the previous syntax. This works as
         an escape from '#{' and '}#' The 'backquote' functionality is
         lost.
- '.' :: Reads a solely '.' as a symbol. The 'consing dot'
         functionality is lost.
- '\' :: The '\' becomes a ordinally constituent character. The
         'multiple escaping' functionality is lost.
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

*** Level 3 (insane)
This is used when ~level~ is 3 or ~:insane~.

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
- '/' :: '/', '//', or '/='

And, '//' means a line comment, '/* ... */' means a block comment.

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
Changes *readtable*.

* Notes
There is no support for trigraphs or digraphs.

* See Also
~with-c-syntax~, ~unuse-reader~.
"
  (unless (translate-reader-level level)
    (error "Level ~S cannot be accepted" level))
  (assert (member case '(nil :upcase :downcase :preserve :invert)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push (list :level ,level :case ,case :previous *readtable*)
           *current-c-reader*)
     (setf *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\{ #'read-toplevel-in-c-syntax)
     *readtable*))

(defmacro unuse-reader ()
  "* Syntax
~unuse-reader~ <no arguments> => readtable

* Arguments and Values
- readtable :: a readtable

* Description
This macro disposes the c reader established by ~use-reader~, and
restores the previous readtable.

* Side Effects
Changes *readtable*.

* See Also
~unuse-reader~.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((prev-reader (pop *current-c-reader*)))
       (setf *readtable* (getf prev-reader :previous)))))

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
