(in-package :with-c-syntax)

(defun test-pointer ()
  (eval-equal 1 ()
    {
    int x = 1 \;
    int * p = & x \;
    int * q = & * p \;
    return * q \;
    })
  t)
