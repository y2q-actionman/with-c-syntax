(in-package #:with-c-syntax)

(defun preprocessor-initial-set ()
  (flet ((make-entry (sym)
           (cons (symbol-name sym) sym)))
    (nconc (mapcar #'make-entry +operators+)
           (mapcar #'make-entry +keywords+))))

(defun preprocessor-initial-set-for-upcase ()
  (flet ((make-entry (sym)
           (let ((ucase (string-upcase (symbol-name sym))))
             (if (string/= sym ucase)
                 (cons ucase sym)
                 nil))))
    (delete nil
            (nconc (mapcar #'make-entry +operators+)
                   (mapcar #'make-entry +keywords+)))))

(defvar *preprocessor-macro*
  (preprocessor-initial-set))

(defvar *preprocessor-macro-for-upcase*
  (preprocessor-initial-set-for-upcase))

(defun preprocessor-macro-compare (x y)
  (if (and (symbolp x) (symbolp y))
      (eq x y)
      (string= x y)))

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
         (return (values (apply fn args)
                         lis-head)))))

;; TODO: recursive expansion
(defun preprocessor (lis &key allow-upcase-keyword)
  (loop with ret = nil
     with typedef-hack = nil
     for i = (pop lis)
     while i
     ;; preprocessor macro
     when (symbolp i)
     do (let* ((entry
                (or (assoc i *preprocessor-macro*
                           :test #'preprocessor-macro-compare)
                    (if allow-upcase-keyword
                        (assoc i *preprocessor-macro-for-upcase*
                               :test #'preprocessor-macro-compare))))
               (name (car entry))
               (val (cdr entry)))
          (when entry
            (cond ((null val)           ; no-op
                   nil)                   
                  ((functionp val)    ; preprocessor funcion
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

     ;; typedef hack -- addes "void \;" after each typedef.
     if (eq (first ret) '|typedef|)
     do (setf typedef-hack t)
     else if (and typedef-hack
                  (eq (first ret) '\;))
     do (setf typedef-hack nil)
       (push '|void| ret)
       (push '\; ret)
     end

     finally
       (return (nreverse ret))))

(defun define-preprocessor-macro (name val &optional for-upcase)
  ;; TODO: cleanup..
  (if for-upcase
      (if-let ((entry (assoc name *preprocessor-macro-for-upcase*
                             :test #'preprocessor-macro-compare)))
        (setf (cdr entry) val)
        (push (cons name val) *preprocessor-macro-for-upcase*))
      (if-let ((entry (assoc name *preprocessor-macro*
                             :test #'preprocessor-macro-compare)))
        (setf (cdr entry) val)
        (push (cons name val) *preprocessor-macro*))))
