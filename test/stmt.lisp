(in-package #:with-c-syntax.test)

;;; expressions

(test test-const
  (is.equal.wcs 1
    return 1 \;)
  (is.equal.wcs #\a
    return #\a \;)
  (is.equal.wcs 1.0
    return 1.0 \;))

(test test-primary-exp
  (is.equal.wcs 'hoge
    return 'hoge \;)
  (is.equal.wcs "abc"
    return "abc" \;)
  (is.equal.wcs 111
    return \( 111 \) \;)
  ;; lisp expression
  (is.equal.wcs 3
    return (+ 1 2) \;))

(test test-postfix-exp
  ;; aref
  (let ((arr (make-array 5 :initial-contents '(0 1 2 3 4))))
    (is.equal.wcs 2
      return arr [ 2 ] \;))
  ;; funcall
  (is.equal.wcs '(1 2 3)
    return list \( 1 \, 2 \, 3 \) \;)
  (is.equal.wcs '()
    return list \( \) \;)
  ;; struct ref
  (is.equal.wcs 99
    {
    struct hoge { int x \; } \;
    struct hoge x = { 99 } \;
    return x \. x \;
    })
  (is.equal.wcs 99
    {
    struct hoge { int x \; } \;
    struct hoge x = { 99 } \;
    return \( & x \) -> x \;
    })
  ;; post increment/decrement
  (let ((hoge 0))
    (is.equal.wcs 0
      return hoge ++ \;)
    (is (= hoge 1))
    (is.equal.wcs 1
      return hoge -- \;)
    (is (= hoge 0))))

(test test-unary-exp
  ;; pre increment/decrement
  (let ((hoge 0))
    (is.equal.wcs 1
      return ++ hoge \;)
    (is (= hoge 1))
    (is.equal.wcs 0
      return -- hoge \;)
    (is (= hoge 0)))
  ;; '&' and '*'
  (let ((hoge 99))
    (is.equal.wcs 99
      return * & hoge \;)
    (is.equal.wcs 99
      return * & hoge \;))
  ;; '+' and '-'
  (is.equal.wcs -1
    return + -1 \;)
  (is.equal.wcs 1
      return - -1 \;)
  ;; '!'
  (let ((hoge nil))
    (is.equal.wcs t
      return ! hoge \;)
    (is.equal.wcs nil
      return ! ! hoge \;))
  (is.equal.wcs 1
    return sizeof 2 \;)
  (is.equal.wcs 1
    return sizeof \( int \) \;))

(test test-cast-exp
  (let ((hoge 10))
    (is.equal.wcs 10
      { return \( int \) hoge \; })
    (is.equal.wcs 10
      { return \( int \) \( int * \) hoge \; })))

(test test-mult-exp
  (is.equal.wcs 18
    return 6 * 3 \;)
  (is.equal.wcs 2
    return 6 / 3 \;)
  (is.equal.wcs 1
    return 3 % 2 \;))

(test test-addictive-exp
  (is.equal.wcs 5
    return 3 + 2 \;)
  (is.equal.wcs 1
    return 3 - 2 \;))

(test test-shift-exp
  (is.equal.wcs 8
    return 4 << 1 \;)
  (is.equal.wcs 2
    return 4 >> 1 \;))

(test test-relational-exp
  (is.equal.wcs t
    return 1 < 2 \;)
  (is.equal.wcs nil
    return 1 > 2 \;)
  (is.equal.wcs t
    return 1 <= 2 \;)
  (is.equal.wcs nil
    return 1 >= 2 \;))

(test test-equality-exp
  (is.equal.wcs nil
    return 1 == 2 \;)
  (is.equal.wcs t
    return 1 != 2 \;))

(test test-and-exp
  (is.equal.wcs #b0001
    return #b0011 & #b0101 \;))

(test test-exclusive-or-exp
  (is.equal.wcs #b0110
    return #b0011 ^ #b0101 \;))

(test test-inclusive-or-exp
  (is.equal.wcs #b0111
    return #b0011 \| #b0101 \;))

(test test-logical-and-exp
  (muffle-unused-code-warning
    (is.equal.wcs nil
      return (or) && 'b \;))
  (is.equal.wcs 'b
    return 'a && 'b \;))

(test test-logical-or-exp
  (muffle-unused-code-warning
    (is.equal.wcs 'a
      return 'a \|\| 'b \;))
  (is.equal.wcs 'b
    return (or) \|\| 'b \;))

(test test-conditional-exp
  (muffle-unused-code-warning
    (is.equal.wcs 'then
      return (and) ? 'then \: 'else \;)
    (is.equal.wcs 'else
      return (or) ? 'then \: 'else \;)))

(test test-assignment-exp
  (let ((x nil) (y 2))
    (is.equal.wcs 2
      return x = y \;)
    (is (= x 2))
    (is.equal.wcs 4
      return x *= y \;)
    (is (= x 4))
    (is.equal.wcs 2
      return x /= y \;)
    (is (= x 2))
    (is.equal.wcs 0
      return x %= y \;)
    (is (= x 0))
    (is.equal.wcs 2
      return x += y \;)
    (is (= x 2))
    (is.equal.wcs 0
      return x -= y \;)
    (is (= x 0))

    (setf x 1)
    (is.equal.wcs 4
      return x <<= y \;)
    (is (= x 4))
    (is.equal.wcs 1
      return x >>= y \;)
    (is (= x 1))

    (setf x #b0011 y #b0101)
    (is.equal.wcs #b0001
      return x &= y \;)
    (is (= x #b0001))

    (setf x #b0011 y #b0101)
    (is.equal.wcs #b0110
      return x ^= y \;)
    (is (= x #b0110))

    (setf x #b0011 y #b0101)
    (is.equal.wcs #b0111
      return x \|= y \;)
    (is (= x #b0111))))

(test test-exp
  (is.equal.wcs 'z
    return 'x \, 'y \, 'z \;))


;;; statements

(test test-labeled-stmt
  (is.equal.wcs 'some-stmt
    a \: return 'some-stmt \;)
  (is.equal.wcs 'some-stmt
    case 100 \: return 'some-stmt \;)
  (is.equal.wcs 'some-stmt
    default \: return 'some-stmt \;))

(test test-exp-stmt
  (is.equal.wcs nil
    { \; }))

(test test-compound-stmt
  (let ((x 0))
    (is.equal.wcs 3
      { x ++ \; x ++ \; x ++ \; return x \; }))
  (is.equal.wcs nil
    {  }))

(test test-selection-stmt
  (is.equal.wcs 'then
    if \( (and) \) return 'then \; )
  (muffle-unused-code-warning
    (is.equal.wcs 'nil
      if \( (or) \) return 'then \; )
    (is.equal.wcs 'then
      if \( (and) \) return 'then \; else return 'else \;)
    (is.equal.wcs 'else
      if \( (or) \) return 'then \; else return 'else \;))

  (flet ((switch-test (x)
	   (with-c-syntax ()
	     switch \( x \) {
             case 1 \: return 'hoge \;
             case 2 \: return 'fuga \;
             default \: return 'piyo \;
	     })))
    (is (eq 'hoge (switch-test 1)))
    (is (eq 'fuga (switch-test 2)))
    (is (eq 'piyo (switch-test 3)))))

(test test-iteration-stmt
  (let ((x 0))
    (is.equal.wcs 100
      while \( 1 \) {
      ++ x \;
      if \( x >= 100 \) return x \;
      }))

  (let ((x 0))
    (is.equal.wcs 100
      {
      while \( x < 100 \)
        ++ x \;
      return x \;
      }))
  (let ((x 1))
    (is.equal.wcs 2
      {
      do {
        ++ x \;
      } while \( x < 0 \) \;
      return x \;
      }))
  ;; 'for' and its variations
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      for \( i = 1 \; i <= 100 \; ++ i \)
        ret += i \;
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      for \( i = 1 \; i <= 100 \; \) {
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      for \( i = 0 \; \; ++ i \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      for \( i = 0 \; \; \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))

  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      i = 0 \;
      for \( \; i <= 100 \; ++ i \)
        ret += i \;
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      i = 0 \;
      for \( \; i <= 100 \; \) {
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      i = 0 \;
      for \( \; \; ++ i \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (is.equal.wcs 5050
      {
      i = 0 \;
      for \( \; \; \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
  	++ i \;
      }
      return ret \;
      })))

(test test-jump-stmt
  ;; (simple) goto
  (muffle-unused-code-warning
    (is.equal.wcs 'y
      {
      goto y \;
      x \: return 'x \;
      y \: return 'y \;
      }))
  ;; break, continue
  (let ((i nil) (ret 0))
    (muffle-unused-code-warning
      (is.equal.wcs 5050
        {
        for \( i = 1 \; \; ++ i \) {
  	  if \( ! \( i <= 100 \) \) break \;
          ret += i \;
          continue \;
          (assert nil () "never comes here") \;
        }
        return ret \;
        })))
  ;; return
  (is.equal.wcs 1
    return 1 \; )
  (is.equal.wcs nil
    return \;))

;;; Extensions

(test test-stat-expr
  (is.equal.wcs -1
    {
    \( { -1 \; } \) \;
    })
  (is.equal.wcs 0
    {
    return \( { 0 \; } \) \;
    })
  (is.equal.wcs 1
    {
    int x = \( { 1 \; } \) \;
    return x \;
    })
  (is.equal.wcs 3
    {
    int h = \( { int x = 1 \, y = 2 \; x + y \; } \) \;
    return h \;
    })
  (is.equal.wcs 211
    {
    int i = 4 \;
    int j = \( { int x = 1 \, y = 2 \;
                 i += 100 \;
                 x + y + i \; } \) \;
    j += i \;
    return j \;
    }))
