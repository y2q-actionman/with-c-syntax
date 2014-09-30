(in-package :with-c-syntax)

(defun test-pointer-of-dereference ()
  (eval-equal 1 ()
    {
    int x = 1 \;
    int * p = & x \;
    int * q = & * p \;
    return * q \;
    })
  t)

(defun test-pointer-to-struct ()
  ;; pointer to a struct
  (eval-equal t ()
    {
    struct hoge { int x \; int y \; int z \; } \;
    struct hoge foo = { 0 \, 0 \, 0 } \;
    struct hoge * p = & foo \;
    p -> y = 999 \;
    return foo \. x == 0 && foo \. y == 999 && foo \. z == 0 \;
    })
  ;; pointer to a struct member
  (eval-equal t ()
    {
    struct hoge { int x \; int y \; int z \; } \;
    struct hoge foo = { 0 \, 0 \, 0 } \;
    int * p = & foo \. z \;
    * p = 999 \;
    return foo \. x == 0 && foo \. y == 0 && foo \. z == 999 \;
    })
  t)

(defun test-pointer-to-array ()
  (eval-equal t ()
    {
    int hoge [ 3 ] = { 0 \, 1 \, 2 } \;
    int * p = & hoge [ 0 ] \;
    * p = 100 \;
    * \( p + 1 \) = 101 \;
    return hoge [ 0 ] == 100 && hoge [ 1 ] == 101 && hoge [ 2 ] == 2 \;
    })

  (eval-equal t ()
    {
    int hoge [ 3 ] = { 0 \, 1 \, 2 } \;
    int * p = & hoge [ 0 ] \;
    return p [ 0 ] == 0 && p [ 1 ] == 1 && p [ 2 ] == 2 \;
    })

  (eval-equal t ()
    {
    int hoge [ 3 ] = { 0 \, 1 \, 2 } \;
    int * p = & hoge [ 0 ] \;
    int * q = p \;
    * p = 100 \;
    p ++ \;
    * p = 101 \;
    p ++ \;
    * p = 102 \;
    return q [ 0 ] == 100 && q [ 1 ] == 101 && q [ 2 ] == 102 \;
    })

  (eval-equal 4 ()
    {
    int hoge [ 3 ] [ 3 ]
      = { { 0 \, 1 \, 2 } \, { 3 \, 4 \, 5 } \, { 6 \, 7 \, 8 } } \;
    int \( * p \) [ ] = & hoge [ 1 ] \;
    return p [ 0 ] [ 1 ] \;
    })

  t)

(defun test-add (x y)
  (+ x y))

(defun test-pointer-to-func ()
  ;; normal call
  (eval-equal 3 ()
    return test-add \( 1 \, 2 \) \;
    )
  ;; other forn
  (eval-equal 3 ()
    return #'test-add \( 1 \, 2 \) \;
    )
  ;; func-ptr
  (eval-equal 3 ()
    {
    int \( * funcptr \) \( \) \;
    funcptr = #'test-add \;
    return funcptr \( 1 \, 2 \) \;
    })
  t)


(defun test-pointer ()
  (test-pointer-of-dereference)
  (test-pointer-to-struct)
  (test-pointer-to-array)
  (test-pointer-to-func)
  t)

