(in-package #:with-c-syntax)

;; This file contains the runtime respesentation of 'struct' in
;; with-c-syntax, named "wcs-struct".

;;; wcs-struct type
(defclass wcs-struct ()
  ((field-index-table :initarg :field-index-table)
   (fields :initarg :fields :initform #()
           :accessor wcs-struct-fields)))

(defun make-wcs-struct (spec-obj &rest init-args)
  (etypecase spec-obj
    (wcs-struct-spec t)
    (symbol (setf spec-obj
		  (or (find-global-wcs-struct-spec spec-obj)
		      (error "no wcs-struct defined: ~S" spec-obj)))))
  (loop with default-args = (wcs-struct-spec-initforms spec-obj)
     with field-index-alist = (wcs-struct-spec-field-index-alist spec-obj)
     with size = (length field-index-alist)
     with init-args-len = (length init-args)
     with ret = (make-instance
                 'wcs-struct
                 :field-index-table (alist-hash-table field-index-alist)
                 :fields (make-array `(,size)))
     for idx from 0 below size
     for (init1 . inits) = init-args then inits
     for (default1 . defaults) = default-args then defaults
     do (setf (aref (wcs-struct-fields ret) idx)
              (if (< idx init-args-len) init1 default1))
     finally (return ret)))

(defun wcs-struct-field-index (wcs-struct field-name)
  (let* ((table (slot-value wcs-struct 'field-index-table))
         (index (gethash field-name table)))
    (unless index
      (error "field ~S not found" field-name))
    index))

(defun wcs-struct-field (wcs-struct field-name)
  (let ((idx (wcs-struct-field-index wcs-struct field-name)))
    (aref (wcs-struct-fields wcs-struct) idx)))

(defun (setf wcs-struct-field) (val wcs-struct field-name)
  (let ((idx (wcs-struct-field-index wcs-struct field-name)))
    (setf (aref (wcs-struct-fields wcs-struct) idx) val)))
