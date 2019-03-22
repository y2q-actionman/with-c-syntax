(in-package #:with-c-syntax.test)

(test test-stdlib-float
  ;; well-known value
  (is.equal.wcs single-float-epsilon
    return FLT_EPSILON \;)
  (is.equal.wcs least-positive-normalized-single-float
    return FLT_MIN \;)
  (is.equal.wcs most-positive-single-float
    return FLT_MAX \;)

  (is.equal.wcs double-float-epsilon
    return DBL_EPSILON \;)
  (is.equal.wcs least-positive-normalized-double-float
    return DBL_MIN \;)
  (is.equal.wcs most-positive-double-float
    return DBL_MAX \;)

  (is.equal.wcs long-float-epsilon
    return LDBL_EPSILON \;)
  (is.equal.wcs least-positive-normalized-long-float
    return LDBL_MIN \;)
  (is.equal.wcs most-positive-long-float
    return LDBL_MAX \;)

  (is.equal.wcs short-float-epsilon
    return SFLT_EPSILON \;)
  (is.equal.wcs least-positive-normalized-short-float
    return SFLT_MIN \;)
  (is.equal.wcs most-positive-short-float
    return SFLT_MAX \;)

  ;; C required limits.
  (is.equal.wcs t
    return FLT_RADIX >= 2 \;)
  (is.equal.wcs t
    return DBL_RADIX >= 2 \;)
  (is.equal.wcs t
    return LDBL_RADIX >= 2 \;)

  (is.equal.wcs t
    return FLT_DIG >= 6 \;)
  (is.equal.wcs t
    return DBL_DIG >= 10 \;)
  (is.equal.wcs t
    return LDBL_DIG >= 10 \;)

  (is.equal.wcs t
    return FLT_MIN_10_EXP <= -37 \;)
  (is.equal.wcs t
    return DBL_MIN_10_EXP <= -37 \;)
  (is.equal.wcs t
    return LDBL_MIN_10_EXP <= -37 \;)

  (is.equal.wcs t
    return FLT_MAX_10_EXP >= 37 \;)
  (is.equal.wcs t
    return DBL_MAX_10_EXP >= 37 \;)
  (is.equal.wcs t
    return LDBL_MAX_10_EXP >= 37 \;))

(test test-stdlib-iso646
  (is.equal.wcs t
    {
    int i = 7 \, j = 3 \;
    return \( i && j \) == \( i |and| j \) \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 7 \;
    i &= 3 \;
    j |and_eq| 3 \;
    return i == j \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 3 \;
    return \( i & j \) == \( i |bitand| j \) \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 3 \;
    return \( i \| j \) == \( i |bitor| j \) \;
    })
  (is.equal.wcs t
    {
    int i = 7 \;
    return ~ i == |compl| i \;
    })
  (is.equal.wcs t
    {
    int i = 7 \;
    return eq \( ! i \, |not| i \) \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 3 \;
    return eq \( i != j \, i |not_eq| j \) \;
    })
  (muffle-unused-code-warning
    (is.equal.wcs t
      {
      int i = 7 \, j = 3 \;
      return \( i \|\| j \) == \( i |or| j \) \;
      }))
  (is.equal.wcs t
    {
    int i = 7 \, j = 7 \;
    i \|= 3 \;
    j |or_eq| 3 \;
    return i == j \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 3 \;
    return \( i ^ j \) == \( i |xor| j \) \;
    })
  (is.equal.wcs t
    {
    int i = 7 \, j = 7 \;
    i ^= 3 \;
    j |xor_eq| 3 \;
    return i == j \;
    }))

(test test-stdlib-limits
  (is.equal.wcs t
    return CHAR_BIT >= 8 \;)
  (muffle-unused-code-warning
    (is.equal.wcs t
      return CHAR_MAX == SCHAR_MAX \|\| CHAR_MAX == UCHAR_MAX \;)
    (is.equal.wcs t
      return CHAR_MIN == SCHAR_MIN \|\| CHAR_MIN == 0 \;))
  (is.equal.wcs t
    return INT_MAX >= 32767 \;)
  (is.equal.wcs t
    return INT_MIN <= -32767 \;)
  (is.equal.wcs t
    return SHRT_MAX >= 32767 \;)
  (is.equal.wcs t
    return SHRT_MIN <= -32767 \;)
  (is.equal.wcs t
    return LONG_MAX >= 2147483647 \;)
  (is.equal.wcs t
    return LONG_MIN <= -2147483647 \;)
  (is.equal.wcs t
    return LLONG_MAX >= 9223372036854775807 \;)
  (is.equal.wcs t
    return LLONG_MIN <= -9223372036854775807 \;)
  (is.equal.wcs t
    return SCHAR_MAX >= 127 \;)
  (is.equal.wcs t
    return SCHAR_MIN <= -127 \;)
  (is.equal.wcs t
    return UCHAR_MAX >= 255 \;)
  (is.equal.wcs t
    return UINT_MAX >= 65535 \;)
  (is.equal.wcs t
    return USHRT_MAX >= 65535 \;)
  (is.equal.wcs t
    return ULONG_MAX >= 4294967295 \;)
  (is.equal.wcs t
    return ULLONG_MAX >= 18446744073709551615 \;)
  (is.equal.wcs t
    return MB_LEN_MAX >= 1 \;))

(test test-stdlib-stddef
  (is.equal.wcs 0
    return NULL \;)
  (is.equal.wcs 1
    {
    ptrdiff_t x = 1 \;
    return x \;
    })
  (is.equal.wcs 2
    {
    size_t x = 2 \;
    return x \;
    })
  (is.equal.wcs 3
    {
    wchar_t x = 3 \;
    return x \;
    })

  (signals.macroexpand.wcs ()
    return offsetof \( int \, i \) \;)
  (signals.macroexpand.wcs ()
    return offsetof \( struct s \, i \) \;)
    
  (is.equal.wcs t
    {
    struct s {
        int i \;
        char c \;
        double d \;
        char a [ 0 ] \;
    } \;

    struct s dummy \;
    \( void \) dummy \;

    return offsetof \( struct s \, i \) >= 0
     && offsetof \( struct s \, c \) >= offsetof \( struct s \, i \)
     && offsetof \( struct s \, d \) >= offsetof \( struct s \, c \)
     && offsetof \( struct s \, a \) >= offsetof \( struct s \, d \) \;
    }))

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
