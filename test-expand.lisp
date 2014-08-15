(in-package :cl-user)

;; (parse-with-lexer (list-lexer '(x * - - 2 + 3 * y)) *expression-parser*)
;; => (+ (* X (- (- 2))) (* 3 Y))	       


#|
(with-c-syntax ()
{
  return 1 + 2 \;
}
)
3

(defparameter x 0)
(with-c-syntax () {
  while \( x < 100 \)
    x ++ \;
  return x \;
}
)

(defparameter i 0)
(with-c-syntax ()
{
  for \( i = 0 \; i < 100 \; ++ i \)
  (format t "~A~%" i) \;
}
)

(defparameter i 0)
(with-c-syntax ()
{
  for \( i = 0 \; i < 100 \; ++ i \) {
    if \( (oddp i) \)
      continue \;
    if \( i == 50 \)
      break \;
    (format t "~A~%" i) \;
  }
}
)

(with-c-syntax ()
{
  switch \( x \) {
  case 1 \:
    (format t "case 1 ni kita~%") \;
    break \;
  case 2 \:
    (format t "case 2 ni kita~%") \;
    (format t "fall-though~%") \;
  default \:
    (format t "default ni kita~%") \;
  }
}
)


(with-c-syntax ()
{
  goto a \;
  a \:
    return 100 \;
}
)

;; removed dereference temporarylly..
(with-c-syntax ()
{
	n = \( count + 7 \) / 8 \;
	switch \( count % 8 \) {
	case 0 \:	do {	to = from ++ \;
	case 7 \:		to = from ++ \;
	case 6 \:		to = from ++ \;
	case 5 \:		to = from ++ \;
	case 4 \:		to = from ++ \;
	case 3 \:		to = from ++ \;
	case 2 \:		to = from ++ \;
	case 1 \:		to = from ++ \;
		} while \( -- n > 0 \) \;
	}
}
)

|#
