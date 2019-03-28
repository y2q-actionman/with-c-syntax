(in-package #:with-c-syntax.test)

;;; translation-unit

(test test-trans-decl-simple
  (is.equal.wcs nil
    int *a* \; )
  (is (boundp '*a*))
  (is.equal.wcs nil
    int *a* \; int *b* \; )
  (is (boundp '*a*))
  (is (boundp '*b*)))

(test test-trans-fdefinition-simple
  (with-c-syntax ()
    int hoge1 \( x \, y \)
      int x \, y \;
    { return x + y \; })
  (is (fboundp 'hoge1))
  (is (= 3 (hoge1 1 2)))

  (with-c-syntax ()
    hoge2 \( x \, y \)
      int x \, y \;
    { return x + y \; })
  (is (fboundp 'hoge2))
  (is (= 3 (hoge2 1 2)))

  (with-c-syntax ()
    int hoge3 \( \)
    { return 3 \; })
  (is (fboundp 'hoge3))
  (is (= 3 (hoge3)))

  (with-c-syntax ()
    int hoge4 \( x \)
    { return x + 4 \; })
  (is (fboundp 'hoge4))
  (is (= 9 (hoge4 5)))

  (with-c-syntax ()
    hoge5 \( \)
    { return 5 \; })
  (is (fboundp 'hoge5))
  (is (= 5 (hoge5)))

  (with-c-syntax ()
    hoge6 \( x \)
    { return x + 6 \; })
  (is (fboundp 'hoge6))
  (is (= 12 (hoge6 6)))

  (with-c-syntax ()
    hoge7 \( int x \, float y \)
    { return x + y \; })
  (is (fboundp 'hoge7))
  (is (<= 5 (hoge7 5 0.4) 6))

  (with-c-syntax ()
    struct test { int x \; } \;
    hoge8 \( x \) {
      struct test s = { x } \;
      s \. x *= 8 \;
      return s \. x \;
    })
  (is (make-struct 'test))
  (is (fboundp 'hoge8))
  (is (= 16 (hoge8 2)))

  (with-c-syntax ()
    int hoge9 \( int \)
    { return 9 \; })
  (is (fboundp 'hoge9))
  (is (= 9 (hoge9 'a)))

  t)

(test test-trans-decl-static
  (is.equal.wcs.option (:return xxx)
      99
    static int xxx = 99 \; )
  (is (not (boundp 'xxx)))
  (with-c-syntax ()
    static int xxx = 0 \;
    int reset-a \( \) {
       xxx = 0 \;
       return xxx \;
    }
    int inc-a \( \) {
       return ++ xxx \;
    })
  (is (not (boundp 'xxx)))
  (is (fboundp 'reset-a))
  (is (fboundp 'inc-a))
  (is (= 1 (inc-a)))
  (is (= 2 (inc-a)))
  (is (= 3 (inc-a)))
  (is (= 4 (inc-a)))
  (is (= 0 (reset-a))))

(test test-stmt-static
  ;; This is `:statement' expansion, so `*xxx*' expanded into toplevel form.
  (is.equal.wcs 0
    (with-c-syntax ()
      {
      static int xxx = 0 \;
      return xxx \;
      })))

(test test-trans-fdefinition-varargs
  (signals.wcs ()
    va_list ap \;
    va_start \( ap \, cnt \) \;)
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
  (is (fboundp 'sumn))
  (is (= 0 (sumn 0)))
  (is (= 3 (sumn 3 1 1 1)))
  (is (= 10 (sumn 4 1 2 3 4))))

(test test-trans-fdefinition-and-storage-class
  (is.equal.wcs.option (:return (s-func 1 2))
      3
    static int s-func \( x \, y \)
    int x \, y \;
    { return x + y \; })
  (is (not (fboundp 's-func))))

(test test-trans-func-local-static
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
  (is (fboundp 'accumulator))
  (is (= 100 (accumulator 0)))
  (is (= 101 (accumulator 1)))
  (is (= 103 (accumulator 2)))
  (is (= 0 (accumulator -1)))
  (is (= 100 (accumulator 100))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-c-syntax ()
    enum { SOME_CONSTANT_100 = 100 } \;))

(test test-trans-other-unit-enum
  (is.equal.wcs 100
    return SOME_CONSTANT_100 \;))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-c-syntax ()
    struct xxx-struct { int x \; } \;))

(test test-trans-other-unit-struct
  (is (make-struct 'xxx-struct))
  (with-c-syntax ()
    hoge \( x \) {
      struct xxx-struct s = { x } \;
      s \. x *= 8 \;
      return s \. x \;
    })
  (is (= 16 (hoge 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (remove-typedef 'int_xxx_t)
  (with-c-syntax ()
    typedef int int_xxx_t \;))

(test test-trans-other-unit-typedef
  (is (find-typedef 'int_xxx_t))
  (with-c-syntax ()
    int_xxx_t hoge \( x \) {
      int_xxx_t tmp = x \;
      return tmp \;
    })
  (is (= 2 (hoge 2))))

;; TODO: add tests for toplevel pointer usage:
;; (with-c-syntax:with-c-syntax ()
;;   int wcs-duff-device-2 \( int to-seq \, int from-seq \, int cnt \) {
;;    int * to = & to-seq \;
;;    })

;; TODO: add more 'const' tests.
#+ ()
(test test-stmt-const
  (is.equal.wcs 0
    (with-c-syntax (:return xxx)
      const int xxx = 0 \;))
  (is.equal.wcs 1
    (with-c-syntax ()
      {
      static const int xxx = 1 \;
      return xxx \;
      }))
  (is.equal.wcs 3
    (with-c-syntax ()
      {
      static const int xxx = 2 \;
      return ++ xxx \;
      })))
