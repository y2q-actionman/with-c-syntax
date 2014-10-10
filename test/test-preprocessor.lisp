(in-package #:with-c-syntax.test)

(defun test-stdlib-float ()
  ;; well-known value
  (eval-equal single-float-epsilon ()
    return FLT_EPSILON \;)
  (eval-equal least-positive-normalized-single-float ()
    return FLT_MIN \;)
  (eval-equal most-positive-single-float ()
    return FLT_MAX \;)

  (eval-equal double-float-epsilon ()
    return DBL_EPSILON \;)
  (eval-equal least-positive-normalized-double-float ()
    return DBL_MIN \;)
  (eval-equal most-positive-double-float ()
    return DBL_MAX \;)

  (eval-equal long-float-epsilon ()
    return LDBL_EPSILON \;)
  (eval-equal least-positive-normalized-long-float ()
    return LDBL_MIN \;)
  (eval-equal most-positive-long-float ()
    return LDBL_MAX \;)

  (eval-equal short-float-epsilon ()
    return SFLT_EPSILON \;)
  (eval-equal least-positive-normalized-short-float ()
    return SFLT_MIN \;)
  (eval-equal most-positive-short-float ()
    return SFLT_MAX \;)

  ;; C required limits.
  (eval-equal t ()
    return FLT_RADIX >= 2 \;)
  (eval-equal t ()
    return DBL_RADIX >= 2 \;)
  (eval-equal t ()
    return LDBL_RADIX >= 2 \;)

  (eval-equal t ()
    return FLT_DIG >= 6 \;)
  (eval-equal t ()
    return DBL_DIG >= 10 \;)
  (eval-equal t ()
    return LDBL_DIG >= 10 \;)

  (eval-equal t ()
    return FLT_MIN_10_EXP <= -37 \;)
  (eval-equal t ()
    return DBL_MIN_10_EXP <= -37 \;)
  (eval-equal t ()
    return LDBL_MIN_10_EXP <= -37 \;)

  (eval-equal t ()
    return FLT_MAX_10_EXP >= 37 \;)
  (eval-equal t ()
    return DBL_MAX_10_EXP >= 37 \;)
  (eval-equal t ()
    return LDBL_MAX_10_EXP >= 37 \;)
  t)

(defun test-stdlib-iso646 ()
  (eval-equal t ()
    {
    int i = 7 \, j = 3 \;
    return \( i && j \) == \( i |and| j \) \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 7 \;
    i &= 3 \;
    j |and_eq| 3 \;
    return i == j \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 3 \;
    return \( i & j \) == \( i |bitand| j \) \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 3 \;
    return \( i \| j \) == \( i |bitor| j \) \;
    })
  (eval-equal t ()
    {
    int i = 7 \;
    return ~ i == |compl| i \;
    })
  (eval-equal t ()
    {
    int i = 7 \;
    return eq \( ! i \, |not| i \) \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 3 \;
    return eq \( i != j \, i |not_eq| j \) \;
    })
  (muffle-unused-code-warning
    (eval-equal t ()
      {
      int i = 7 \, j = 3 \;
      return \( i \|\| j \) == \( i |or| j \) \;
      }))
  (eval-equal t ()
    {
    int i = 7 \, j = 7 \;
    i \|= 3 \;
    j |or_eq| 3 \;
    return i == j \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 3 \;
    return \( i ^ j \) == \( i |xor| j \) \;
    })
  (eval-equal t ()
    {
    int i = 7 \, j = 7 \;
    i ^= 3 \;
    j |xor_eq| 3 \;
    return i == j \;
    })
  t)

(defun test-stdlib-limits ()
  (eval-equal t ()
    return CHAR_BIT >= 8 \;)
  (muffle-unused-code-warning
    (eval-equal t ()
      return CHAR_MAX == SCHAR_MAX \|\| CHAR_MAX == UCHAR_MAX \;)
    (eval-equal t ()
      return CHAR_MIN == SCHAR_MIN \|\| CHAR_MIN == 0 \;))
  (eval-equal t ()
    return INT_MAX >= 32767 \;)
  (eval-equal t ()
    return INT_MIN <= -32767 \;)
  (eval-equal t ()
    return SHRT_MAX >= 32767 \;)
  (eval-equal t ()
    return SHRT_MIN <= -32767 \;)
  (eval-equal t ()
    return LONG_MAX >= 2147483647 \;)
  (eval-equal t ()
    return LONG_MIN <= -2147483647 \;)
  (eval-equal t ()
    return LLONG_MAX >= 9223372036854775807 \;)
  (eval-equal t ()
    return LLONG_MIN <= -9223372036854775807 \;)
  (eval-equal t ()
    return SCHAR_MAX >= 127 \;)
  (eval-equal t ()
    return SCHAR_MIN <= -127 \;)
  (eval-equal t ()
    return UCHAR_MAX >= 255 \;)
  (eval-equal t ()
    return UINT_MAX >= 65535 \;)
  (eval-equal t ()
    return USHRT_MAX >= 65535 \;)
  (eval-equal t ()
    return ULONG_MAX >= 4294967295 \;)
  (eval-equal t ()
    return ULLONG_MAX >= 18446744073709551615 \;)
  (eval-equal t ()
    return MB_LEN_MAX >= 1 \;)
  t)

(defun test-stdlib-stdbool ()
  (eval-equal 1 ()
    {
    bool x = 1 \;
    return x \;
    })
  (eval-equal 1 ()
    return |__bool_true_false_are_defined| \;)
  (eval-equal t ()
    return true \;)
  (eval-equal nil ()
    return false \;)
  t)

(defun test-stdlib-stddef ()
  (eval-equal 1 ()
    {
    bool x = 1 \;
    return x \;
    })
  (eval-equal 1 ()
    return |__bool_true_false_are_defined| \;)
  (eval-equal t ()
    return true \;)
  (eval-equal nil ()
    return false \;)
  t)

(defun test-stdlib-stddef ()
  (eval-equal 0 ()
    return NULL \;)
  (eval-equal 1 ()
    {
    ptrdiff_t x = 1 \;
    return x \;
    })
  (eval-equal 2 ()
    {
    size_t x = 2 \;
    return x \;
    })
  (eval-equal 3 ()
    {
    wchar_t x = 3 \;
    return x \;
    })
  t)

(defun test-preprocessor ()
  (test-stdlib-float)
  (test-stdlib-iso646)
  (test-stdlib-limits)
  (test-stdlib-stdbool)
  (test-stdlib-stddef)
  t)
