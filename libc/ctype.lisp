(in-package #:with-c-syntax.libc-implementation)

;;; My hand-crafted codes assume 'C' locale.

(defun |isalnum| (char)
  (alphanumericp char))

(defun |isalpha| (char)
  (alpha-char-p char))

(defun |islower| (char)
  (lower-case-p char))

(defun |isupper| (char)
  (upper-case-p char))

(defun |isdigit| (char)
  (digit-char-p char))

(defun |isxdigit| (char)
  (digit-char-p char 16))

(defun |iscntrl| (char)			; assumes 'C' locale.
  (let ((code (char-code char)))
    (or (<= 0 code #x1F)
	(= code #x7F))))

(defun |isgraph| (char)
  (and (char/= char #\space)		; Common Lisp treats whitespace as graphic.
       (graphic-char-p char)))

(defun |isspace| (char)			; assumes 'C' locale.
  (case char
    ((#\space #\page #\newline #\return #\tab ; These are whitespace in Lisp and C both.
	      #\linefeed		; Only for Lisp.
	      #. (code-char #x0b))	; Vertical tab -- only for C.
      t)
    (otherwise nil)))

(defun |isblank| (char)			; assumes 'C' locale.
  (case char
    ((#\space #\tab) t)
    (otherwise nil)))

(defun |isprint| (char)			; assumes 'C' locale.
  (not (|iscntrl| char)))

(defun |ispunct| (char)			; assumes 'C' locale.
  (and (|isgraph| char)
       (not (|isalnum| char))))

(defun |tolower| (char)
  (char-downcase char))

(defun |toupper| (char)
  (char-upcase char))
