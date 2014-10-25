(in-package #:with-c-syntax.test)

(defun test-redundant-wcs ()
  (eval-equal nil ()
    )
  (eval-equal 1 ()
    (with-c-syntax ()
      (with-c-syntax ()
	return 1 \; )))
  (eval-equal 1 ()
    (with-c-syntax ()
      (with-c-syntax ()
	(with-c-syntax ()
	  return 1 \; ))))
  t)

(defun test-auto-add-{} ()
  (eval-equal 10 ()
    int x = 10 \;
    return x \;)
  (assert-compile-error (:try-add-{} nil)
    (eval-equal 10 ()
      int x = 10 \;
      return x \;))
  t)

(defun test-wcs-option-combine ()
  (eval-equal 1 (:entry-form xxx)
    (with-c-syntax ()
      static int xxx = 1 \;))
  t)
      

(defun test-wcs ()
  (test-redundant-wcs)
  (test-auto-add-{})
  ;; TODO: add keyword arg combination test
  t)
