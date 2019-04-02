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

(defun |strcpy| (dst src)
  (replace (adjust-array dst (length src))
	   src))

(defun |strncpy| (dst src count)
  (let ((src-len (length src))
	(ret (adjust-array dst count)))
    (replace ret src)
    (when (> count src-len)
      (fill ret (code-char 0) :start (1+ src-len)))
    ret))
