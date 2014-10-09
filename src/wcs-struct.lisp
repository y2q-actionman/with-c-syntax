(in-package #:with-c-syntax)

;; This file contains the runtime respesentation of 'struct' in
;; with-c-syntax, named "wcs-struct".

;;; wcs-struct runtime spec
(defclass wcs-struct-runtime-spec ()
  ((index-alist :initarg :index-alist
                :reader runtime-spec-index-alist)
   (default-initforms :initarg :default-initforms
     :reader runtime-spec-default-initforms)
   (size :initarg :size
         :reader runtime-spec-size)))

(defun make-wcs-struct-runtime-spec (field-names union-p default-initforms)
  (loop for f in field-names
     for idx from 0
     collect (cons f (if union-p 0 idx)) into ia-list
     finally (return (make-instance
                      'wcs-struct-runtime-spec
                      :index-alist ia-list
                      :size (if union-p 1 (1+ idx))
		      :default-initforms default-initforms))))

(defmethod make-load-form ((obj wcs-struct-runtime-spec) &optional env)
  (declare (ignore env))
  ;; TODO: apply MAKE-LOAD-FORM to default-initforms recursively?
  `(make-instance 'wcs-struct-runtime-spec
                  :index-alist ',(slot-value obj 'index-alist)
                  :default-initforms ',(slot-value obj 'default-initforms)
                  :size ,(slot-value obj 'size)))

;;; wcs-struct runtime spec repository
(defvar *wcs-struct-runtime-spec-alist* nil)

(defun install-wcs-struct-runtime-spec (name wcsspec)
  (if-let ((entry (assoc name *wcs-struct-runtime-spec-alist* :test #'eq)))
    (setf (cdr entry) wcsspec)
    (push (cons name wcsspec) *wcs-struct-runtime-spec-alist*)))

(defun find-wcs-struct-runtime-spec (name)
  (car (assoc name *wcs-struct-runtime-spec-alist* :test #'eq)))

;;; wcs-struct type
(defclass wcs-struct ()
  ((field-index-table :initarg :field-index-table)
   (fields :initarg :fields :initform #()
           :accessor wcs-struct-fields)))

(defun make-wcs-struct (spec-obj &rest init-args)
  (etypecase spec-obj
    (wcs-struct-runtime-spec t)
    (symbol (setf spec-obj
		  (or (find-wcs-struct-runtime-spec spec-obj)
		      (error "no wcs-struct defined: ~S" spec-obj)))))
  (loop with size = (runtime-spec-size spec-obj)
     with default-args = (runtime-spec-default-initforms spec-obj)
     with init-args-len = (length init-args)
     with ret = (make-instance 'wcs-struct
                               :field-index-table
                               (alist-hash-table (runtime-spec-index-alist spec-obj)
                                :test #'eq)
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
