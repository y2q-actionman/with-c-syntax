(in-package #:with-c-syntax)

(defvar *preprocessor-symbol-macro* nil)

(defun preprocessor-call-macro (lis-head name fn)
  ;; firstly, finds first '('
  (let ((begin (pop lis-head)))
    (unless (string= begin '|(|)
      (error "some symbols (~S) found between preprocessor macro (~S) and the first '('"
             begin name)))
  (flet ((get-arg ()
           (loop for i = (pop lis-head)
              if (null lis-head)
              do (error "reached end of symbols at finding preprocessor macro args")
              else if (string= i '|)|)
              do (push i lis-head)
                (loop-finish)
              else if (string= i '|,|)
              do (loop-finish)
              else
              collect i)))
    (loop as next = (first lis-head)
       if (null next)
       do (error "reached end of symbols at finding preprocessor macro args")
       else if (string= next '|)|)
       do (pop lis-head)
         (loop-finish)
       else
       collect (get-arg) into args
       finally
         (pprint args)
         (return (values (apply fn args)
                         lis-head)))))

(defun preprocessor (lis &key allow-upcase-keyword)
  (loop with ret = nil
     for i = (pop lis)
     while i
     ;; keyword case conversion
     when allow-upcase-keyword
     do(when (symbolp i)
         (when-let
             ((op (or (member i +operators+
                              :key #'string-upcase
                              :test #'string=)
                      (member i +keywords+
                              :key #'string-upcase
                              :test #'string=))))
           (push (car op) ret)
           (setf i nil)))
       (when (and (listp i)
                  (string= (first i) (string-upcase '|type|)))
         (push `(|type| ,@(rest i)) ret)
         (setf i nil))
     ;; preprocessor macro
     when (symbolp i)
     do (let* ((entry (assoc i *preprocessor-symbol-macro*
                             :test #'string=))
               (name (car entry))
               (val (cdr entry)))
          (when entry
            (cond ((functionp val)    ; preprocessor funcion
                   (multiple-value-bind (ex-val new-lis)
                       (preprocessor-call-macro lis name val)
                     (push ex-val ret)
                     (setf lis new-lis)
                     (setf i nil)))
                  (t                  ; symbol expansion
                   (push val ret)
                   (setf i nil)))))
     ;; otherwise..
     when i
     do (push i ret)

     finally
       (return (nreverse ret))))

(defun define-preprocessor-symbol (name val)
  (if-let ((entry (assoc name *preprocessor-symbol-macro*
                         :test #'string=)))
    (setf (cdr entry) val)
    (push (cons name val) *preprocessor-symbol-macro*)))
