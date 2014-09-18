(in-package :with-c-syntax)

;; These are referenced by the parser directly.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append-item-to-right (lis i)
    (append lis (list i)))

  (defun concatinate-comma-list (lis op i)
    (declare (ignore op))
    (append-item-to-right lis i))
)

(define-modify-macro appendf (&rest args)
  append)

(define-modify-macro nconcf (&rest args)
  nconc)

(define-modify-macro append-item-to-right-f (i)
  append-item-to-right)

(defmacro with-dynamic-bound-symbols ((&rest symbols) &body body)
  `(progv ',symbols (list ,@symbols)
     (locally (declare (special ,@symbols))
       ,@body)))
