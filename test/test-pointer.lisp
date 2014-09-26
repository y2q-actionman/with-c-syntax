(in-package :with-c-syntax)

(defun test-pointer ()
  (eval-equal 1 ()
    {
    int x = 1 \;
    int * p = & x \;
    int * q = & * p \;
    return * q \;
    })
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
