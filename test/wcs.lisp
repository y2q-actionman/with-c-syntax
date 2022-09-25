(in-package #:with-c-syntax.test)

(test test-redundant-wcs
  (is (equal
       nil
       (with-c-syntax ())))
  (is (equal
       1
       (with-c-syntax ()
	 (with-c-syntax ()
	   return 1 \; ))))
  (is (equal
       1
       (with-c-syntax ()
	 (with-c-syntax ()
	   (with-c-syntax ()
	     return 1 \; ))))))

(test test-auto-add-{}
  (is.equal.wcs 10
    int x = 10 \;
    return x \;)
  (signals with-c-syntax-error
    (macroexpand
     '(with-c-syntax (:try-add-{} nil)
       int x = 10 \;
       return x \;))))

(test test-wcs-option-combine
  (is (equal
       1
       (with-c-syntax (:preprocess t)   ; override
	 (with-c-syntax (:preprocess :preprocess-only)
	   { static int xxx = 1 \; return xxx \; }))))
  ;; TODO: add keyword arg combination test more!
  )
