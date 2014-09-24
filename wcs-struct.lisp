(in-package :with-c-syntax)

;; This file contains the runtime respesentation of 'struct' in
;; with-c-syntax, named "wcs-struct".

;;; wcs-struct funcs
(defclass wcs-struct-runtime-spec ()
  ((index-alist :initarg :index-alist
                :reader runtime-spec-index-alist)
   (size :initarg :size
         :reader runtime-spec-size)))

(defun make-wcs-struct-runtime-spec (field-list union-p)
  (loop for i in field-list
     for idx from 0
     collect (cons i (if union-p 0 idx)) into ia-list
     finally (return (make-instance
                      'wcs-struct-runtime-spec
                      :index-alist ia-list
                      :size (if union-p 1 (length field-list))))))

(defmethod make-load-form ((obj wcs-struct-runtime-spec) &optional env)
  `(make-instance 'wcs-struct-runtime-spec
                  :index-alist ',(slot-value obj 'index-alist)
                  :size ,(slot-value obj 'size)))

;;; wcs-struct type
(defclass wcs-struct ()
  ((field-index-table :initarg :field-index-table)
   (fields :initarg :fields :initform #()
           :accessor wcs-struct-fields)))

(defvar *wcs-struct-lisp-type* 'wcs-struct)

(defun wcs-struct-lisp-type-p (obj)
  (eq obj 'wcs-struct))

(defun make-wcs-struct (spec-obj &rest args)
  (loop with size = (runtime-spec-size spec-obj)
     with ret = (make-instance 'wcs-struct
                               :field-index-table
                               (alexandria:alist-hash-table
                                (runtime-spec-index-alist spec-obj)
                                :test #'eq)
                               :fields (make-array `(,size)))
     for idx from 0 below size
     for arg in args
     do (setf (aref (wcs-struct-fields ret) idx) arg)
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
