(in-package #:with-c-syntax.stdlib)

(defun va_start (ap last)
  (declare (type list ap))
  (unless (length= 1 ap)
    (error 'library-macro-error
           :name "va_start" :args (list ap last)))
  `(setf ,(first ap) (get-variadic-arguments)))

(defun va_arg (ap type)
  (declare (type list ap))
  (unless (length= 1 ap)
    (error 'library-macro-error
           :name "va_arg" :args (list ap type)))
  `(pop ,(first ap)))

(defun va_end (ap)
  (declare (type list ap))
  (unless (length= 1 ap)
    (error 'library-macro-error
           :name "va_end" :args (list ap)))
  `(setf ,(first ap) nil))

(defun va_copy (dest src)
  (declare (type list dest src))
  (unless (and (length= 1 dest)
               (length= 1 src))
    (error 'library-macro-error
           :name "va_copy" :args (list dest src)))
  `(setf ,(first dest) (copy-list ,(first src))))

(eval-when (:load-toplevel :execute)
  (add-predefined-typedef-and-aliases '|va_list| 'list)
  (add-preprocessor-macro-with-upcase "va_start" #'va_start)
  (add-preprocessor-macro-with-upcase "va_arg" #'va_arg)
  (add-preprocessor-macro-with-upcase "va_end" #'va_end)
  (add-preprocessor-macro-with-upcase "va_copy" #'va_copy)
  )
