(in-package #:with-c-syntax)

(define-constant +reader-level-specifier-alist+
    '((0 . 0) (1 . 1) (2 . 2) (3 . 3)
      (:conservative . 0) (:aggressive . 1)
      (:overkill . 2) (:insane . 3))
  :test 'equal)

(defun translate-reader-level (rlspec)
  (cdr (assoc rlspec +reader-level-specifier-alist+
	      :test #'eq)))

(defvar *current-c-reader* nil
  "a list of plists. the plist is:
:level     -> specified default reader level.
:case      -> specified default reader case.
:previous  -> the original readtable.")

(defun read-in-previous-syntax (stream char n)
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
       (loop do
	  (peek-char #\* stream t nil t)
	  (read-char stream t nil t)
	  (when (char= #\/ (read-char stream t nil t))
	    (loop-finish)))
       (values))
      (t
       (read-single-or-equal-symbol stream char)))))

(defun install-c-reader (readtable level)
  (let ((lev (translate-reader-level level)))
    (unless lev
      (error "Level ~S cannot be accepted" level))
    (setf level lev))
  ;; accessing normal syntax
  (set-dispatch-macro-character #\# #\! #'read-in-previous-syntax readtable)
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
    ;; Disables 'consing dots', with replacement of ()
    (set-macro-character #\. #'read-lonely-single-symbol t readtable)
    ;; removes 'multi-escape'
    (set-syntax-from-char #\| #\& readtable)
    ;; destroys CL syntax COMPLETELY!
    (set-macro-character #\' #'read-single-quote nil readtable)
    (set-macro-character #\; #'read-single-character-symbol nil readtable)
    (set-macro-character #\( #'read-single-character-symbol nil readtable)
    (set-macro-character #\) #'read-single-character-symbol nil readtable))
  (when (>= level 3)			; Insane
    ;; No compatibilities between CL symbols.
    (set-macro-character #\? #'read-single-character-symbol nil readtable)
    (set-macro-character #\~ #'read-single-character-symbol nil readtable)
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
    (set-macro-character #\: #'read-single-character-symbol nil readtable)
    (set-macro-character #\. #'read-single-character-symbol nil readtable))
  readtable)

(defun read-toplevel-in-c-syntax (stream char n)
  (destructuring-bind (&key previous level case &allow-other-keys)
      (first *current-c-reader*)
    (let* ((*readtable* (copy-readtable previous))
	   (keyword-case (or case (readtable-case *readtable*))))
      (setf (readtable-case *readtable*) keyword-case)
      (set-dispatch-macro-character #\# #\{ #'read-toplevel-in-c-syntax)
      (install-c-reader *readtable* (or n level))
      `(with-c-syntax (:keyword-case ,keyword-case)
	 ,@(read-2chars-delimited-list
	    (cdr (assoc char +bracket-pair-alist+ :test #'eq))
	    #\# stream t)))))

(defmacro use-reader (&key (level :overkill) case)
  (unless (translate-reader-level level)
    (error "Level ~S cannot be accepted" level))
  (assert (member case '(nil :upcase :downcase :preserve :invert)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push (list :level ,level :case ,case :previous *readtable*)
           *current-c-reader*)
     (setf *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\{ #'read-toplevel-in-c-syntax)))

(defmacro unuse-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((prev-reader (pop *current-c-reader*)))
       (setf *readtable* (getf prev-reader :previous)))))

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
