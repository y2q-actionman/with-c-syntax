(in-package :with-c-syntax)

;;; translation-unit

(defun test-trans-decl-simple ()
  (eval-equal nil ()
    int a \; )
  (eval-equal nil ()
    int a \; int b \; )
  t)

;; TODO: support 'any' entry point
(defun test-trans-fdefinition-simple ()
  (with-c-syntax ()
    int hoge1 \( x \, y \)
      int x \, y \;
    { return x + y \; }
   )
  (assert (= 3 (hoge1 1 2)))

  (with-c-syntax ()
    hoge2 \( x \, y \)
      int x \, y \;
    { return x + y \; }
    )
  (assert (= 3 (hoge2 1 2)))

  (with-c-syntax ()
    int hoge3 \( \)
    { return 3 \; }
    )
  (assert (= 3 (hoge3)))

  (with-c-syntax ()
    int hoge4 \( x \)
    { return x + 4 \; }
    )
  (assert (= 9 (hoge4 5)))

  (with-c-syntax ()
    hoge5 \( \)
    { return 5 \; } 
    )
  (assert (= 5 (hoge5)))

  (with-c-syntax ()
    hoge6 \( x \)
    { return x + 6 \; }
    )
  (assert (= 12 (hoge6 6)))

  (with-c-syntax ()
    hoge7 \( int x \, float y \)
    { return x + y \; }
    )
  (assert (<= 5 (hoge7 5 0.4) 6))

  (with-c-syntax ()
    struct test { int x \; } \;
    hoge8 \( x \) {
      struct test s = { x } \;
      s \. x *= 8 \;
      return s \. x \;
    }
    )
  (assert (= 16 (hoge8 2)))

  t)

(defun test-trans-decl-static ()
  (eval-equal nil ()
    static int xxx \; )
  (eval-equal 'inc-a ()
    static int xxx = 0 \;
    int inc-a \( \) {
       return ++ xxx \;
    })
  (assert (= 1 (inc-a)))
  (assert (= 2 (inc-a)))
  (assert (= 3 (inc-a)))
  (assert (= 4 (inc-a)))
  t)

;; TODO: add variadic arg test

(defun test-trans ()
  (test-trans-decl-simple)
  (test-trans-fdefinition-simple)
  t)
