(in-package :with-c-syntax)

(defun test (form)
  (format t "~&~S~%   ~S~%" form 
	  (c-expression-tranform form ())))
