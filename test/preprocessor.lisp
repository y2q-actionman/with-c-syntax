(in-package #:with-c-syntax.test)

(test test-collect-preprocessor-macro-arguments
  (flet ((cpma (x)
	   (with-c-syntax.core::collect-preprocessor-macro-arguments x)))
    (is (equal (cpma '(|(| 1 2 3 |)|))
	       '((1 2 3))))
    (is (equal (cpma '(|(| 1 |,| 2 |,| 3 |)|))
	       '((1) (2) (3))))
    (is (equal (cpma '(|(| int a |,| int b |,| |(| a b c |)| |)|))
	       '((int a) (int b) (|(| a b c |)|))))
    (is (equal (cpma '(|(| |)|))
	       '()))))

(test test-strcat
  (is.equal.wcs "abc"
    return "a" "b" "c" \; ))

(test test-typedef-hack ()
  (is.equal.wcs 1
    {
    typedef int int_t \;
    int_t x = 1 \;
    return x \;
    }))

;;; TODO: add symbol-interning tests
