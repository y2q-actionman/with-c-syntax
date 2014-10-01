(in-package :with-c-syntax)

;; Thanks to https://gist.github.com/chaitanyagupta/9324402

(defvar *readtable-history* nil)

(defun read-single-character-symbol (stream char)
  (declare (ignore stream))
  (intern (string char)))

(defun make-string-from-chars (&rest chars)
  (coerce (apply #'vector chars) 'string))

(defun read-single-or-equal-character-symbol (stream char)
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

(defun read-minus-symbol (stream char)
  (let ((next (read-char stream t nil t)))
    (case next
      ((#\= #\> #\-)
       (let ((str (coerce (vector char next) 'string)))
	 (intern str)))
      (t
       (when next
	 (unread-char next stream))
       (intern (string char))))))

(defun read-shift-symbol (stream char)
  (let ((next (read-char stream t nil t)))
    (cond ((char= next char)		; shift op
	   (let ((next2 (read-char stream t nil t)))
	     (case next2
	       (#\=
		(intern (make-string-from-chars char next next2)))
	       (t
		(when next2
		  (unread-char next2 stream))
		(intern (make-string-from-chars char next))))))
	  ((char= next #\=)		; compare op
	   (intern (make-string-from-chars char next)))
	  (t
	   (when next
	     (unread-char next stream))
	   (intern (string char))))))

;; TODO:
;; * cares abount destroyed syntaxes
;;   , |
;; 
;; * difficult chars
;;   ( ) . :
;; 
;; * constituent trais (numeric prefix, etc)
;;   + -
;; 
;; * C-style comments?
;;   /


(defun enable-wcs-reader ()
  (push *readtable* *readtable-history*)
  (setf *readtable* (copy-readtable))
  ;; simple operator
  (set-macro-character #\, #'read-single-character-symbol) ; TODO
  (set-macro-character #\? #'read-single-character-symbol)
  (set-macro-character #\~ #'read-single-character-symbol)
  (set-macro-character #\[ #'read-single-character-symbol)
  (set-macro-character #\] #'read-single-character-symbol)
  (set-macro-character #\: #'read-single-character-symbol t) ; TODO
  ;; maybe followed by '='
  (set-macro-character #\= #'read-single-or-equal-character-symbol)
  (set-macro-character #\* #'read-single-or-equal-character-symbol)
  (set-macro-character #\% #'read-single-or-equal-character-symbol)
  (set-macro-character #\^ #'read-single-or-equal-character-symbol)
  (set-macro-character #\! #'read-single-or-equal-character-symbol)
  ;; some complex op
  (set-macro-character #\+ #'read-single-or-equal-or-self-symbol t)
  (set-macro-character #\& #'read-single-or-equal-or-self-symbol)
  (set-macro-character #\| #'read-single-or-equal-or-self-symbol) ; TODO
  (set-macro-character #\- #'read-minus-symbol t)
  (set-macro-character #\< #'read-shift-symbol)
  (set-macro-character #\> #'read-shift-symbol)
  ;; simple syntactic symbol
  (set-macro-character #\; #'read-single-character-symbol)
  (set-macro-character #\{ #'read-single-character-symbol)
  (set-macro-character #\} #'read-single-character-symbol)
  *readtable*)

(defun disable-wcs-reader ()
  (setf *readtable* (pop *readtable-history*)))
  