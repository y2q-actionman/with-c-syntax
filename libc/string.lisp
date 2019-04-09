(in-package #:with-c-syntax.libc)

;;; My hand-crafted codes assume 'C' locale.

;;; TODO
;;; - memchr
;;; - memcmp
;;; - memcpy
;;; - memmove
;;; - memset
;;; - strcoll
;;; - strerror
;;; - strtok
;;; - strxfrm

(defun resize-string (string size)
  "Resize STRING to SIZE using `adjust-array' or `fill-pointer'.
This function is used for emulating C string truncation with NUL char."
  (declare (type string string)
	   (type fixnum size))
  (let ((str-size (length string)))
    (cond ((= str-size size)
	   string)			; return itself.
	  ((and (array-has-fill-pointer-p string)
		(< size (array-total-size string)))
	   (setf (fill-pointer string) size)
	   string)
	  (t
	   (adjust-array string size
			 :fill-pointer (if (array-has-fill-pointer-p string)
					   size))))))

(define-modify-macro resize-stringf (size)
  resize-string
  "Modify macro of `resize-string'")


(defun |strcpy| (dst src)
  "Emulates 'strcpy' of the C language."
  ;; To emulate C's string truncation with NUL char, I use `adjust-array'.
  (resize-stringf dst (length src))
  (replace dst src))

(defun |strncpy| (dst src count)
  "Emulates 'strncpy' of the C language."
  (resize-stringf dst count)
  (replace dst src)
  ;; zero-filling of 'strncpy'.
  (let ((src-len (length src)))
    (when (> count src-len)
      (fill dst (code-char 0) :start src-len)))
  dst)

(defun |strcat| (dst src)
  "Emulates 'strcat' of the C language."
  (|strncat| dst src (length src)))

(defun |strncat| (dst src count)
  "Emulates 'strncat' of the C language."
  (let ((dst-len (length dst))
	(src-cat-len (min (length src) count) ))
    (resize-stringf dst (+ dst-len src-cat-len))
    (replace dst src :start1 dst-len :end2 src-cat-len)))

(defun |strlen| (str)
  "Emulates 'strlen' of the C language."
  (length str))

(defun strcmp* (str1 str1-len str2 str2-len)
  "Used by `|strcmp|' and `|strncmp|'"
  (let ((mismatch (string/= str1 str2 :end1 str1-len :end2 str2-len)))
    (cond
      ((null mismatch) 0)
      ((string< str1 str2 :start1 mismatch :start2 mismatch
		:end1 str1-len :end2 str2-len)
       (- (1+ mismatch))) ; I use `1+' to avoid confusion between 0 as equal and 0 as index
      (t		  ; `string>'
       (1+ mismatch)))))

(defun |strcmp| (str1 str2)
  "Emulates 'strcmp' of the C language."
  (strcmp* str1 nil str2 nil))

(defun |strncmp| (str1 str2 count)
  "Emulates 'strncmp' of the C language."
  (strcmp* str1 (min count (length str1))
	   str2 (min count (length str2))))

(defun make-trimed-string (string trim-position)
  "Trims the head TRIM-POSITION chars of STRING, using `displaced-array'.
If TRIM-POSITION is nil, returns NIL instead. (I think this behavior
works better with the `position' functions family.)

This function is used for emulating C string truncation with pointer movements."
  ;; (assert (null (array-displacement string)))
  (cond ((null trim-position)
	 (values nil trim-position))
	((zerop trim-position)
	 (values string trim-position))
	(t
	 (make-array (- (length string) trim-position)
		     :element-type 'character
		     :displaced-to string
		     :displaced-index-offset trim-position))))

(defun |strchr| (str ch)
  "Emulates 'strchr' of the C language."
  (make-trimed-string str
		      (position ch str)))
;;; TODO: add test

(defun |strrchr| (str ch)
  "Emulates 'strrchr' of the C language."
  (make-trimed-string str
		      (position ch str :from-end t)))
;;; TODO: add test

(defun |strspn| (str accept)
  "Emulates 'strspn' of the C language."
  (position-if-not (lambda (c) (find c accept))
		   str))
;;; TODO: add test

(defun |strcspn| (str reject)
  "Emulates 'strcspn' of the C language."
  (position-if (lambda (c) (find c reject))
	       str))
;;; TODO: add test

(defun |strpbrk| (str accept)
  "Emulates 'strpbrk' of the C language."
  (make-trimed-string str (|strspn| str accept)))
;;; TODO: add test

(defun |strstr| (haystack needle)
  "Emulates 'strstr' of the C language."
  (if-let (i (search needle haystack))
    (make-trimed-string-with-displaced-array haystack i)
    nil))
