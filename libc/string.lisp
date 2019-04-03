(in-package #:with-c-syntax.libc)

;;; My hand-crafted codes assume 'C' locale.

;;; TODO
;;; - memchr
;;; - memcmp
;;; - memcpy
;;; - memmove
;;; - memset
;;; - strcar
;;; - strncat
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
	   (adjust-array string size)))))

(define-modify-macro resize-stringf (size)
  resize-string
  "Modify macro of `resize-string'")


(defun |strcpy| (dst src)
  (check-type dst string "the first argument of 'strcpy' must be a string.")
  (check-type src string "the first argument of 'strcpy' must be a string.")
  ;; To emulate C's string truncation with NUL char, I use `adjust-array'.
  (resize-stringf dst (length src))
  (replace dst src))

(defun |strncpy| (dst src count)
  (check-type dst string "the first argument of 'strncpy' must be a string.")
  (check-type src string "the second argument of 'strncpy' must be a string.")
  (check-type count fixnum "the third argument of 'strncpy' must be a fixnum.")
  (resize-stringf dst count)
  (replace dst src)
  (let ((src-len (length src)))
    (when (> count src-len)
      (fill dst (code-char 0) :start (1+ src-len))))
  dst)
