(in-package :with-c-syntax)

;;; expressions

(defun test-const ()
  (eval-equal 1 ()
    return 1 \;)
  (eval-equal #\a ()
    return #\a \;)
  (eval-equal 1.0 ()
    return 1.0 \;)
  t)

(defun test-primary-exp ()
  (eval-equal 'hoge ()
    return 'hoge \;)
  (test-const)
  (eval-equal "abc" ()
    return "abc" \;)
  (eval-equal 111 ()
    return \( 111 \) \;)
  ;; lisp expression
  (eval-equal 3 ()
    return (+ 1 2) \;)
  t)

(defstruct hoge-struct
  (a 'hoge-struct-a))

(defun test-postfix-exp ()
  (test-primary-exp)
  ;; aref
  (let ((arr (make-array 5 :initial-contents '(0 1 2 3 4))))
    (eval-equal 2 ()
      return arr [ 2 ] \;))
  ;; funcall
  (eval-equal '(1 2 3) ()
    return list \( 1 \, 2 \, 3 \) \;)
  (eval-equal '() ()
    return list \( \) \;)
  ;; struct ref
  (let ((hoge-1 (make-hoge-struct)))
    (eval-equal 'hoge-struct-a ()
      return hoge-1 \. hoge-struct-a \;)
    (eval-equal 'hoge-struct-a ((hoge-1 hoge-1))
      return \( & hoge-1 \) -> hoge-struct-a \;))
  ;; post increment/decrement
  (let ((hoge 0))
    (eval-equal 0 ()
      return hoge ++ \;)
    (assert (= hoge 1))
    (eval-equal 1 ()
      return hoge -- \;)
    (assert (= hoge 0)))
  t)

(defun test-unary-exp ()
  (test-postfix-exp)
  ;; pre increment/decrement
  (let ((hoge 0))
    (eval-equal 1 ()
      return ++ hoge \;)
    (assert (= hoge 1))
    (eval-equal 0 ()
      return -- hoge \;)
    (assert (= hoge 0)))
  ;; '&' and '*'
  (let ((hoge 99))
    (eval-equal 99 ((hoge hoge))
      return * & hoge \;))
  ;; '+' and '-'
  (eval-equal -1 ()
    return + -1 \;)
  (eval-equal 1 ()
      return - -1 \;)
  ;; '!'
  (let ((hoge nil))
    (eval-equal t ()
      return ! hoge \;)
    (eval-equal nil ()
      return ! ! hoge \;))
  ;; TODO: sizeof
  ;; TODO: sizeof
  t)

(defun test-cast-exp ()
  (test-unary-exp)
  ;; TODO: (test '({ \( integer \) hoge \; }))
  t)

(defun test-mult-exp ()
  (test-cast-exp)
  (eval-equal 18 ()
    return 6 * 3 \;)
  (eval-equal 2 ()
    return 6 / 3 \;)
  (eval-equal 1 ()
      return 3 % 2 \;)
  t)

(defun test-addictive-exp ()
  (test-mult-exp)
  (eval-equal 5 ()
    return 3 + 2 \;)
  (eval-equal 1 ()
    return 3 - 2 \;)
  t)

(defun test-shift-exp ()
  (test-addictive-exp)
  (eval-equal 8 ()
    return 4 << 1 \;)
  (eval-equal 2 ()
    return 4 >> 1 \;)
  t)

(defun test-relational-exp ()
  (test-shift-exp)
  (eval-equal t ()
    return 1 < 2 \;)
  (eval-equal nil ()
    return 1 > 2 \;)
  (eval-equal t ()
    return 1 <= 2 \;)
  (eval-equal nil ()
    return 1 >= 2 \;)
  t)

(defun test-equality-exp ()
  (test-relational-exp)
  (eval-equal nil ()
    return 1 == 2 \;)
  (eval-equal t ()
    return 1 != 2 \;)
  t)

(defun test-and-exp ()
  (test-equality-exp)
  (eval-equal #b0001 ()
    return #b0011 & #b0101 \;)
  t)

(defun test-exclusive-or-exp ()
  (test-and-exp)
  (eval-equal #b0110 ()
    return #b0011 ^ #b0101 \;)
  t)

(defun test-inclusive-or-exp ()
  (test-exclusive-or-exp)
  (eval-equal #b0111 ()
    return #b0011 \| #b0101 \;)
  t)

(defun test-logical-and-exp ()
  (test-inclusive-or-exp)
  (eval-equal 'b ()
    return 'a && 'b \;)
  t)

(defun test-logical-or-exp ()
  (test-logical-and-exp)
  (eval-equal 'a ()
    return 'a \|\| 'b \;)
  t)

(defun test-conditional-exp ()
  (test-logical-or-exp)
  (eval-equal 'then ()
    return (and) ? 'then \: 'else \;)
  (eval-equal 'else ()
    return (or) ? 'then \: 'else \;)
  t)

(defun test-assignment-exp ()
  (test-conditional-exp)
  (let ((x nil) (y 2))
    (eval-equal 2 ()
      return x = y \;)
    (assert (= x 2))
    (eval-equal 4 ()
      return x *= y \;)
    (assert (= x 4))
    (eval-equal 2 ()
      return x /= y \;)
    (assert (= x 2))
    (eval-equal 0 ()
      return x %= y \;)
    (assert (= x 0))
    (eval-equal 2 ()
      return x += y \;)
    (assert (= x 2))
    (eval-equal 0 ()
      return x -= y \;)
    (assert (= x 0))

    (setf x 1)
    (eval-equal 4 ()
      return x <<= y \;)
    (assert (= x 4))
    (eval-equal 1 ()
      return x >>= y \;)
    (assert (= x 1))

    (setf x #b0011 y #b0101)
    (eval-equal #b0001 ()
      return x &= y \;)
    (assert (= x #b0001))

    (setf x #b0011 y #b0101)
    (eval-equal #b0110 ()
      return x ^= y \;)
    (assert (= x #b0110))

    (setf x #b0011 y #b0101)
    (eval-equal #b0111 ()
      return x \|= y \;)
    (assert (= x #b0111)))
  t)

(defun test-exp ()
  (test-assignment-exp)
  (eval-equal 'z ()
    return 'x \, 'y \, 'z \;)
  t)


;;; statements

(defun test-labeled-stmt ()
  (eval-equal 'some-stmt ()
    a \: return 'some-stmt \;)
  (eval-equal 'some-stmt ()
    case 100 \: return 'some-stmt \;)
  (eval-equal 'some-stmt ()
    default \: return 'some-stmt \;)
  t)

(defun test-exp-stmt ()
  (test-exp)
  (eval-equal nil ()
    { \; })
  t)

(defun test-compound-stmt ()
  (let ((x 0))
    (eval-equal 3 ()
      { x ++ \; x ++ \; x ++ \; return x \; }))
  (eval-equal nil ()
    {  })
  t)

(defun test-selection-stmt ()
  (eval-equal 'then ()
    if \( (and) \) return 'then \; )
  (eval-equal 'nil ()
    if \( (or) \) return 'then \; )
  (eval-equal 'then ()
    if \( (and) \) return 'then \; else return 'else \;)
  (eval-equal 'else ()
    if \( (or) \) return 'then \; else return 'else \;)

  (flet ((switch-test (x)
	   (with-c-syntax ()
	     switch \( x \) {
	      case 1 \: return 'hoge \;
	      case 2 \: return 'fuga \;
    	      default \: return 'piyo \;
	     })))
    (assert (eq 'hoge (switch-test 1)))
    (assert (eq 'fuga (switch-test 2)))
    (assert (eq 'piyo (switch-test 3))))
  t)

(defun test-iteration-stmt ()
  (let ((x 0))
    (eval-equal 100 ()
      {
      while \( x < 100 \)
        ++ x \;
      return x \;
      }))
  (let ((x 1))
    (eval-equal 2 ()
      {
      do {
        ++ x \;
      } while \( x < 0 \) \;
      return x \;
      }))
  ;; for family
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      for \( i = 1 \; i <= 100 \; ++ i \)
        ret += i \;
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      for \( i = 1 \; i <= 100 \; \) {
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      for \( i = 0 \; \; ++ i \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      for \( i = 0 \; \; \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))

  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      i = 0 \;
      for \( \; i <= 100 \; ++ i \)
        ret += i \;
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      i = 0 \;
      for \( \; i <= 100 \; \) {
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      i = 0 \;
      for \( \; \; ++ i \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
      }
      return ret \;
      }))
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      i = 0 \;
      for \( \; \; \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
  	++ i \;
      }
      return ret \;
      }))
  t)

(defun test-jump-stmt ()
  ;; (simple) goto
  (eval-equal 'y ()
    {
      goto y \;
      x \: return 'x \;
      y \: return 'y \;
    })
  ;; break, continue
  (let ((i nil) (ret 0))
    (eval-equal 5050 ()
      {
      for \( i = 1 \; \; ++ i \) {
  	if \( ! \( i <= 100 \) \) break \;
        ret += i \;
	continue \;
	(assert nil () "never comes here") \;
      }
      return ret \;
      }))
  ;; return
  (eval-equal 1 ()
    return 1 \; )
  (eval-equal nil ()
    return \;)
  t)

(defun test-stmt ()
  (test-labeled-stmt)
  (test-exp-stmt)
  (test-compound-stmt)
  (test-selection-stmt)
  (test-iteration-stmt)
  (test-jump-stmt)
  t)
