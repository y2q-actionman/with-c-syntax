(in-package :with-c-syntax)

;; Thanks to https://gist.github.com/chaitanyagupta/9324402

(defvar *readtable-history* nil)

(defun read-in-previous-syntax (stream char n)
  (declare (ignore char n))
  (let ((*readtable* (first *readtable-history*)))
    (read stream t nil t)))

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (intern (string char)))

(defun read-lonely-single-symbol (stream char)
  (let ((next (read-char stream t nil t)))
    (cond ((standard-whitespace-p next)
	   (intern (string char)))
	  (t
	   (unread-char next stream)
	   (let* ((token (read stream t nil t))
		  (buf (format nil "~C~S" char token))
		  (*readtable* (first *readtable-history*)))
	     (read-from-string buf t nil))))))

(defun read-list-no-dot (stream char)
  (declare (ignore char))
  (read-delimited-list #\) stream t))

(defun read-list-alternative (stream char n)
  (declare (ignore char n))
  (let ((*readtable* (copy-readtable))
	(ret nil))
    (loop for lis = (read-delimited-list #\] stream t)
       do (setf ret (nreconc lis ret))
       do (let ((next (read-char stream t nil t)))
	    (when (char= next #\#)
	      (loop-finish))
	    (unread-char next)
	    (push (intern (string next)) ret)))
    (nreverse ret)))

(defun read-single-quote (stream char)
  (loop for c = (read-char stream t nil t)
     while (and c (char/= c char))
     collect c into cs
     finally
       (when (> (length cs) 1)
	 (error "Too many chars appeared within '...' : ~S" cs))
       (return (first cs))))

(defun read-single-or-equal-symbol (stream char)
  (let ((next (read-char stream t nil t)))
    (case next
      (#\=
       (intern (make-string-from-chars char next)))
      (t
       (when next
	 (unread-char next stream))
       (intern (string char))))))

(defun read-single-or-equal-or-self-symbol (stream char)  
   (let ((next (read-char stream t nil t)))
    (cond ((or (char= next char)
	       (char= next #\=))
	   (intern (make-string-from-chars char next)))
 	  (t
	   (when next
	     (unread-char next stream))
	   (intern (string char))))))

(defun read-minus (stream char)
  (let ((next (read-char stream t nil t)))
     (case next
      ((#\= #\> #\-)
       (let ((str (coerce (vector char next) 'string)))
	 (intern str)))
     (t
       (when next
	 (unread-char next stream))
       (intern (string char))))))

(defun read-shift (stream char)
  (let ((next (read-char stream t nil t)))
    (cond ((char= next char)	; shift op
	   (let ((next2 (read-char stream t nil t)))
	     (case next2
	       (#\=
		(intern (make-string-from-chars char next next2)))
	       (t
		(when next2
		  (unread-char next2 stream))
		(intern (make-string-from-chars char next))))))
	  ((char= next #\=)	; compare op
	   (intern (make-string-from-chars char next)))
	  (t
	   (when next
	     (unread-char next stream))
	   (intern (string char))))))

(defun read-slash (stream char)
  (let ((next (peek-char nil stream t nil t)))
    (case next
      (#\/
       (read-line stream t nil t)
       (values))
      (#\*
       (loop for c = (peek-char #\* stream t nil t)
	  when (char= c #\*)
	  do (read-char stream t nil t)
	    (let ((c2 (read-char stream t nil t)))
	      (when (char= c2 #\/)
		(loop-finish))))
       (values))
      (t
       (read-lonely-single-symbol stream char)))))

(defmacro enable-wcs-reader (&key (mode :conservative))
  (let ((level (ecase mode
		 (:conservative 0)
		 (:aggressive 1)
		 (:overkill 2)
		 (:crazy 3))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (push *readtable* *readtable-history*)
       (setf *readtable* (copy-readtable))
       ;; accessing normal syntax
       (set-dispatch-macro-character #\# #\! #'read-in-previous-syntax)
       ;; Conservative: only for well-known parens
       (when (>= ,level 0)
	 (set-macro-character #\{ #'read-single-character-symbol)
	 (set-macro-character #\} #'read-single-character-symbol)
	 (set-macro-character #\[ #'read-single-character-symbol)
	 (set-macro-character #\] #'read-single-character-symbol))
       ;; Aggressive: destroys CL syntax bitly.
       (when (>= ,level 1)
	 ;; Comma is read as a symbol.
	 (set-macro-character #\, #'read-single-character-symbol)
	 ;; Enables solely ':' as a symbol.
	 (set-macro-character #\: #'read-lonely-single-symbol t)
	 ;; Disables 'consing dots'. But only when not in list.
	 (set-macro-character #\. #'read-lonely-single-symbol t)
	 ;; Replaces the list reader. This disbles 'consing dots' completely.
	 (set-macro-character #\( #'read-list-no-dot))
       ;; Overkill: destroys CL syntax COMPLETELY!
       (when (>= ,level 2)
	 (set-dispatch-macro-character #\# #\[ #'read-list-alternative) ; preserves list constructor.
	 ;; removes 'multi-escape'
	 (set-syntax-from-char #\| #\&)
	 ;; now deadly..
	 (set-macro-character #\' #'read-single-quote)
	 (set-macro-character #\; #'read-single-character-symbol)
	 (set-macro-character #\( #'read-single-character-symbol)
	 (set-macro-character #\) #'read-single-character-symbol))
       ;; Crazy: No compatibilities between CL symbols.
       (when (>= ,level 3)
	 (set-macro-character #\? #'read-single-character-symbol)
	 (set-macro-character #\~ #'read-single-character-symbol)
	 (set-macro-character #\= #'read-single-or-equal-symbol)
	 (set-macro-character #\* #'read-single-or-equal-symbol)
	 (set-macro-character #\% #'read-single-or-equal-symbol)
	 (set-macro-character #\^ #'read-single-or-equal-symbol)
	 (set-macro-character #\! #'read-single-or-equal-symbol)
	 (set-macro-character #\+ #'read-single-or-equal-or-self-symbol) ; TODO: numeric
	 (set-macro-character #\& #'read-single-or-equal-or-self-symbol)
	 (set-macro-character #\| #'read-single-or-equal-or-self-symbol)
	 (set-macro-character #\- #'read-minus) ; TODO: numeric
	 (set-macro-character #\< #'read-shift)
	 (set-macro-character #\> #'read-shift)
	 (set-macro-character #\/ #'read-slash)
	 (set-macro-character #\: #'read-single-character-symbol))
       )))

(defmacro disable-wcs-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (pop *readtable-history*))))
