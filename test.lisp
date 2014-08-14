(in-package :cl-user)

(defun test (form)
  (format t "~&~S~%   ~S~%" form 
	  (c-expression-tranform form)))

;;; expressions

(defun test-const ()
  (test '({ 1 \; }))
  (test '({ #\a \; }))
  (test '({ 1.0 \; }))
  ;; TODO: enum
  t)

(defun test-primary-exp ()
  (test '({ hoge \; }))
  (test-const)
  (test '({ "abc" \; }))
  (test '({  \( 111 \)  \; }))
  (test '({ (+ 1 2) \; }))
  t)

(defun test-postfix-exp ()
  (test-primary-exp)
  (test '({ array [ 80 ] \; }))
  (test '({ hoge \( 1 \, 2 \, 3 \) \; }))
  (test '({ hoge \( \) \; }))
  (test '({ hoge \. a \; }))
  (test '({ hoge -> a \; }))
  (test '({ hoge ++ \; }))
  (test '({ fuga -- \; }))
  t)

(defun test-unary-exp ()
  (test-postfix-exp)
  (test '({ ++ hoge \; }))
  (test '({ -- hoge \; }))
  (test '({ & hoge \; }))
  (test '({ * hoge \; }))
  (test '({ + hoge \; }))
  (test '({ - hoge \; }))
  (test '({ ! hoge \; }))
  ;; sizeof
  ;; sizeof
  t)

(defun test-cast-exp ()
  (test-unary-exp)
  ;; (test '({ \( integer \) hoge \; }))
  t)

(defun test-mult-exp ()
  (test-cast-exp)
  (test '({ x * y \; }))
  (test '({ x / y \; }))
  (test '({ x % y \; }))
  t)

(defun test-addictive-exp ()
  (test-mult-exp)
  (test '({ x + y \; }))
  (test '({ x - y \; }))
  t)

(defun test-shift-expression ()
  (test-addictive-exp)
  (test '({ x << y \; }))
  (test '({ x >> y \; }))
  t)

(defun test-relational-exp ()
  (test-shift-expression)
  (test '({ x < y \; }))
  (test '({ x > y \; }))
  (test '({ x <= y \; }))
  (test '({ x >= y \; }))
  t)

(defun test-equality-exp ()
  (test-relational-exp)
  (test '({ x == y \; }))
  (test '({ x != y \; }))
  t)

(defun test-and-exp ()
  (test-equality-exp)
  (test '({ x & y \; }))
  t)

(defun test-exclusive-or-exp ()
  (test-and-exp)
  (test '({ x ^ y \; }))
  t)

(defun test-inclusive-or-exp ()
  (test-exclusive-or-exp)
  (test '({ x \| y \; }))
  t)

(defun test-logical-and-exp ()
  (test-inclusive-or-exp)
  (test '({ x && y \; }))
  t)

(defun test-logical-or-exp ()
  (test-logical-and-exp)
  (test '({ x \|\| y \; }))
  t)

(defun test-conditional-exp ()
  (test-logical-or-exp)
  (test '({ x ? y \: z \; }))
  t)

(defun test-assignment-exp ()
  (test-conditional-exp)
  (test '({ x = y \; }))
  (test '({ x *= y \; }))
  (test '({ x /= y \; }))
  (test '({ x %= y \; }))
  (test '({ x += y \; }))
  (test '({ x -= y \; }))
  (test '({ x <<= y \; }))
  (test '({ x >>= y \; }))
  (test '({ x &= y \; }))
  (test '({ x ^= y \; }))
  (test '({ x \|= y \; }))
  t)

(defun test-exp ()
  (test-assignment-exp)
  (test '({ x \, y \, z \; }))
  t)


;;; statements

(defun test-labeled-stat ()
  (test '({ a \: some_stmt \; }))
  (test '({ case 100 \: some_stmt \; }))
  (test '({ default \: some_stme \; }))
  t)

(defun test-exp-stat ()
  (test-exp)
  (test '({ \; }))
  t)

(defun test-compound-stat ()
  (test '({ x \; y \; z \; }))
  (test '({  }))
  t)

(defun test-selection-stat ()
  (test '({ if \( x \) y \; }))
  (test '({ if \( x \) y \; else z \; }))
  (test '({ switch \( x \) {
	      case 1 \: hoge \; hoge \;
	      case 2 \: fuga \; fuga \;
    	      default \: piyo \; piyo \;
	  } } ))
  t)

(defun test-iteration-stat ()
  (test '({ while \( x \) y \; }))
  (test '({ do { x \; } while \( cond \) \; }))
  (test '({ for \( i = 0 \; i < 100 \; ++ i \) (format t "~A~%" i) \; }))
  (test '({ for \( i = 0 \; i < 100 \;      \) (format t "~A~%" i) \; }))
  (test '({ for \( i = 0 \;         \; ++ i \) (format t "~A~%" i) \; }))
  (test '({ for \( i = 0 \;         \;      \) (format t "~A~%" i) \; }))
  (test '({ for \(       \; i < 100 \; ++ i \) (format t "~A~%" i) \; }))
  (test '({ for \(       \; i < 100 \;      \) (format t "~A~%" i) \; }))
  (test '({ for \(       \;         \; ++ i \) (format t "~A~%" i) \; }))
  (test '({ for \(       \;         \;      \) (format t "~A~%" i) \; }))
  t)

(defun test-jump-stat ()
  (test '({ goto x \; }))
  (test '({ continue \; }))
  (test '({ break \; }))
  (test '({ return 1 \; }))
  (test '({ return \; }))
  t)

(defun test-stat ()
  (test-exp-stat)
  (test-compound-stat)
  (test-selection-stat)
  (test-iteration-stat)
  (test-jump-stat)
  t)
 