(in-package :with-c-syntax)

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

(defun read-again-with-prefix (stream char)
  (let* ((token (read stream t nil t))
	 (buf (format nil "~C~S" char token))
	 (*readtable*
	  (getf (first *current-c-reader*) :previous)))
    (read-from-string buf t nil)))
  
(defun read-lonely-single-symbol (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (cond ((standard-whitespace-p next)
	   (read-char stream t nil t)
	   (intern (string char)))
	  (t
	   (read-again-with-prefix stream char)))))

(defun read-list-no-dot (stream char)
  (declare (ignore char))
  (read-delimited-list #\) stream t))

(defun read-2chars-delimited-list (c1 c2 &optional stream recursive-p)
  (loop for lis = (read-delimited-list c1 stream recursive-p)
     as next = (peek-char nil stream t nil recursive-p)
     nconc lis
     when (char= next c2)
     do (read-char stream t nil recursive-p) (loop-finish)
     else
     collect (intern (string c1))))

(defun read-list-alternative (stream char n)
  (declare (ignore char n))
  (read-2chars-delimited-list #\] #\# stream t))

(defun read-single-quote (stream char)
  (loop for c = (read-char stream t nil t)
     while (and c (char/= c char))
     collect c into cs
     finally
       (when (> (length cs) 1)
	 (error "Too many chars appeared within '...' : ~S" cs))
       (return (first cs))))

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
       (loop for c = (peek-char #\* stream t nil t)
	  when (char= c #\*)
	  do (read-char stream t nil t)	; dispose the peeked one.
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
    (set-macro-character #\] #'read-single-character-symbol nil readtable)
    ;; Disables 'consing dots'. But only when not in list.
    (set-macro-character #\. #'read-lonely-single-symbol t readtable)
    ;; Replaces the list reader. This disables 'consing dots' completely.
    (set-macro-character #\( #'read-list-no-dot nil readtable))
  (when (>= level 2)			; Overkill
    ;; preserves list constructor.
    (set-dispatch-macro-character #\# #\[ #'read-list-alternative readtable)
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

(defun read-in-c-syntax (stream char n)
  (declare (ignore char))
  (destructuring-bind (&key previous level case &allow-other-keys)
      (first *current-c-reader*)
    (let ((*readtable* (copy-readtable previous)))
      (when case
        (setf (readtable-case *readtable*) case))
      (set-dispatch-macro-character #\# #\{ #'read-in-c-syntax)
      (install-c-reader *readtable* (or n level))
      (read-2chars-delimited-list #\} #\# stream t))))

(defmacro use-reader (&key (level :overkill) case)
  (unless (translate-reader-level level)
    (error "Level ~S cannot be accepted" level))
  (assert (member case '(nil :upcase :downcase :preserve :invert)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push (list :level ,level :case ,case :previous *readtable*)
           *current-c-reader*)
     (setf *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\{ #'read-in-c-syntax)))

(defmacro unuse-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((prev-reader (pop *current-c-reader*)))
       (setf *readtable* (getf prev-reader :previous)))))

;;; References at implementation
;;; - https://gist.github.com/chaitanyagupta/9324402
