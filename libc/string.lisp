(in-package #:with-c-syntax.libc)

;;; My hand-crafted codes assume 'C' locale.

;;; TODO
;;; - memchr
;;; - memcmp
;;; - memcpy
;;; - memmove
;;; - memset
;;; - strchr
;;; - strcmp
;;; - strncmp
;;; - strcoll
;;; - strcspn
;;; - strerror
;;; - strlen
;;; - strpbrk
;;; - strrchr
;;; - strspn
;;; - strstr
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
  "Emulates 'strcpy' in the C language."
  ;; To emulate C's string truncation with NUL char, I use `adjust-array'.
  (resize-stringf dst (length src))
  (replace dst src))

(defun |strncpy| (dst src count)
  "Emulates 'strncpy' in the C language."
  (resize-stringf dst count)
  (replace dst src)
  ;; zero-filling of 'strncpy'.
  (let ((src-len (length src)))
    (when (> count src-len)
      (fill dst (code-char 0) :start src-len)))
  dst)

(defun |strcat| (dst src)
  "Emulates 'strcat' in the C language."
  (let ((dst-len (length dst))
	(src-len (length src)))
    (resize-stringf dst (+ dst-len src-len))
    (replace dst src :start1 dst-len)))

(defun |strncat| (dst src count)
  "Emulates 'strncat' in the C language."
  (let ((dst-len (length dst))
	(src-cat-len (min (length src) count) ))
    (resize-stringf dst (+ dst-len src-cat-len))
    (replace dst src :start1 dst-len :end2 src-cat-len)))
