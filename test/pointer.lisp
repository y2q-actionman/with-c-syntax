(in-package #:with-c-syntax.test)

(test test-pointer-of-dereference
  (is.equal.wcs 1
    {
    int x = 1 \;
    int * p = & x \;
    int * q = & * p \;
    return * q \;
    }))

(test test-null-pointer-dereference
  (signals.wcs (with-c-syntax.core::pseudo-pointer-null-dereference-error)
    {
    int * p = NULL \;
    return * p \;
    })
  (is.equal.wcs "NULL-is-FALSE"
    {
    ;; FIXME: current impl ignores init value..
    ;; int * p = nil\;
    int * p \;
    p = nil \;
    if \( p \)
    return "NULL-is-TRUE" \;
    else
    return "NULL-is-FALSE" \;
    })
  (signals.wcs (with-c-syntax.core::pseudo-pointer-dangling-error)
    {
    int x = 1 \;
    int * p = & x \;
    int * q = & * p \;
    with-c-syntax.core:pseudo-pointer-invalidate \( q \) \;
    return * q \;
    })
  )

(test test-pointer-to-struct
  ;; pointer to a struct
  (is.equal.wcs t
    {
    struct hoge { int x \; int y \; int z \; } \;
    struct hoge foo = { 0 \, 0 \, 0 } \;
    struct hoge * p = & foo \;
    p -> y = 999 \;
    return foo \. x == 0 && foo \. y == 999 && foo \. z == 0 \;
    })
  ;; pointer to a struct member
  (is.equal.wcs t
    {
    struct hoge { int x \; int y \; int z \; } \;
    struct hoge foo = { 0 \, 0 \, 0 } \;
    int * p = & foo \. z \;
    * p = 999 \;
    return foo \. x == 0 && foo \. y == 0 && foo \. z == 999 \;
    }))

(test test-pointer-to-array
  (is.equal.wcs t
    {
    int hoge [ 3 ] = { 0 \, 1 \, 2 } \;
    int * p = & hoge [ 0 ] \;
    * p = 100 \;
    * \( p + 1 \) = 101 \;
    return hoge [ 0 ] == 100 && hoge [ 1 ] == 101 && hoge [ 2 ] == 2 \;
    })
  (is.equal.wcs t
    {
    int hoge [ 3 ] = { 0 \, 1 \, 2 } \;
    int * p = & hoge [ 0 ] \;
    return p [ 0 ] == 0 && p [ 1 ] == 1 && p [ 2 ] == 2 \;
    })
  (is.equal.wcs t
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
  (is.equal.wcs 4
    {
    int hoge [ 3 ] [ 3 ]
      = { { 0 \, 1 \, 2 } \, { 3 \, 4 \, 5 } \, { 6 \, 7 \, 8 } } \;
    int \( * p \) [ ] = & hoge [ 1 ] \;
    return p [ 0 ] [ 1 ] \;
    }))

(defun test-add (x y)
  (+ x y))

(test test-pointer-to-func
  ;; normal call
  (is.equal.wcs 3
    return test-add \( 1 \, 2 \) \;)
  ;; other form
  (is.equal.wcs 3
    return #'test-add \( 1 \, 2 \) \;)
  ;; func-ptr
  (is.equal.wcs 3
    {
    int \( * funcptr \) \( \) \;
    funcptr = #'test-add \;
    return funcptr \( 1 \, 2 \) \;
    }))

(test test-pointer-to-local-static
  (with-testing-wcs-bind (get-local-static)
    (with-c-syntax ()
      int get-local-static \( \) {
      static int local_static = 0 \;
      return &local_static \;
      })
    (let ((ptr1 (funcall 'get-local-static))
          (ptr2 (funcall 'get-local-static)))
      (is (= (pseudo-pointer-dereference ptr1)
             (pseudo-pointer-dereference ptr2)))
      (setf (pseudo-pointer-dereference ptr1) 0)
      (is (= (pseudo-pointer-dereference ptr1)
             (pseudo-pointer-dereference ptr2)
             0))
      (setf (pseudo-pointer-dereference ptr2) 999)
      (is (= (pseudo-pointer-dereference ptr1)
             (pseudo-pointer-dereference ptr2)
             (pseudo-pointer-dereference (funcall 'get-local-static))
             999)))))
