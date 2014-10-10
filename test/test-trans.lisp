(in-package #:with-c-syntax.test)

;;; translation-unit

(defun test-trans-decl-simple ()
  (eval-equal nil ()
    int *a* \; )
  (assert (boundp '*a*))
  (eval-equal nil ()
    int *a* \; int *b* \; )
  (assert (boundp '*a*))
  (assert (boundp '*b*))
  t)

(defun test-trans-fdefinition-simple ()
  (with-c-syntax ()
    int hoge1 \( x \, y \)
      int x \, y \;
    { return x + y \; })
  (assert (fboundp 'hoge1))
  (assert (= 3 (hoge1 1 2)))

  (with-c-syntax ()
    hoge2 \( x \, y \)
      int x \, y \;
    { return x + y \; })
  (assert (fboundp 'hoge2))
  (assert (= 3 (hoge2 1 2)))

  (with-c-syntax ()
    int hoge3 \( \)
    { return 3 \; })
  (assert (fboundp 'hoge3))
  (assert (= 3 (hoge3)))

  (with-c-syntax ()
    int hoge4 \( x \)
    { return x + 4 \; })
  (assert (fboundp 'hoge4))
  (assert (= 9 (hoge4 5)))

  (with-c-syntax ()
    hoge5 \( \)
    { return 5 \; })
  (assert (fboundp 'hoge5))
  (assert (= 5 (hoge5)))

  (with-c-syntax ()
    hoge6 \( x \)
    { return x + 6 \; })
  (assert (fboundp 'hoge6))
  (assert (= 12 (hoge6 6)))

  (with-c-syntax ()
    hoge7 \( int x \, float y \)
    { return x + y \; })
  (assert (fboundp 'hoge7))
  (assert (<= 5 (hoge7 5 0.4) 6))

  (with-c-syntax ()
    struct test { int x \; } \;
    hoge8 \( x \) {
      struct test s = { x } \;
      s \. x *= 8 \;
      return s \. x \;
    })
  (assert (with-c-syntax::find-global-wcs-struct-spec 'test))
  (assert (fboundp 'hoge8))
  (assert (= 16 (hoge8 2)))

  (with-c-syntax ()
    int hoge9 \( int \)
    { return 9 \; })
  (assert (fboundp 'hoge9))
  (assert (= 9 (hoge9 'a)))

  t)

(defun test-trans-decl-static ()
  (eval-equal 99 (:entry-form xxx)
    static int xxx = 99 \; )
  (assert (not (boundp 'xxx)))
  (with-c-syntax ()
    static int xxx = 0 \;
    int reset-a \( \) {
       xxx = 0 \;
       return xxx \;
    }
    int inc-a \( \) {
       return ++ xxx \;
    })
  (assert (not (boundp 'xxx)))
  (assert (fboundp 'reset-a))
  (assert (fboundp 'inc-a))
  (assert (= 1 (inc-a)))
  (assert (= 2 (inc-a)))
  (assert (= 3 (inc-a)))
  (assert (= 4 (inc-a)))
  (assert (= 0 (reset-a)))
  t)

(defun test-trans-fdefinition-varargs ()
  (with-c-syntax ()
    int sumn \( int cnt \, |...| \) {
       int i \, ret = 0 \;
       va_list ap \;

       va_start \( ap \, cnt \) \;

       for \( i = 0 \; i < cnt \; i ++ \) {
         ret += va_arg \( ap \, int \) \;
       }

       va_end \( ap \) \;

       return ret \;
    })
  (assert (fboundp 'sumn))
  (assert (= 0 (sumn 0)))
  (assert (= 3 (sumn 3 1 1 1)))
  (assert (= 10 (sumn 4 1 2 3 4)))
  t)

(defun test-trans-fdefinition-and-storage-class ()
  (eval-equal 3 (:entry-form (s-func 1 2))
    static int s-func \( x \, y \)
      int x \, y \;
    { return x + y \; }
   )
  (assert (not (fboundp 's-func)))
  t)

(defun test-trans-func-local-static ()
  (with-c-syntax ()
    int accumulator \( n \) {
       static acc = 100 \;
       if \( n < 0 \) {
          acc = 0 \;
          return 0 \;
       } else {
          return acc += n \;
       }
    })
  (assert (fboundp 'accumulator))
  (assert (= 100 (accumulator 0)))
  (assert (= 101 (accumulator 1)))
  (assert (= 103 (accumulator 2)))
  t)


(defun test-trans ()
  (test-trans-decl-simple)
  (test-trans-fdefinition-simple)
  (test-trans-decl-static)
  (test-trans-fdefinition-varargs)
  (test-trans-fdefinition-and-storage-class)
  t)
