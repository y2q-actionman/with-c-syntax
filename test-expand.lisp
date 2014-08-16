(in-package :cl-user)

(defun test-expand-1 ()
  (with-c-syntax ()
    {
    return 1 + 2 \;
    })
  ;; => 3
  )

(defparameter x 0)

(defun test-expand-2 ()
  (with-c-syntax ()
    {
    while \( x < 100 \)
      x ++ \;
    return x \;
    })
  ;; => 100
  )

(defun test-expand-3 ()
  (let ((i 0))
    (with-c-syntax ()
      {
      for \( i = 0 \; i < 100 \; ++ i \)
      (format t "~A~%" i) \;
      })
    ))

(defun test-expand-4 ()
  (let ((i 0))
    (with-c-syntax ()
      {
      for \( i = 0 \; i < 100 \; ++ i \) {
        if \( (oddp i) \)
          continue \;
        if \( i == 50 \)
          break \;
        (format t "~A~%" i) \;
      }
      })
    ))

(defun test-expand-5 (x)
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
    })
  )


(defun test-expand-6 ()
  (with-c-syntax ()
    {
    goto a \;
    a \:
      return 100 \;
    })
  )

(defun test-expand-7 (xxx &aux z)
  (with-c-syntax (xxx)
    {
    z =  & xxx \;
    (format t "z = ~A~%" z) \;


    (format t "xxx = ~A~%" xxx) \;

    * z += 1 \;
    (format t "xxx = ~A~%" xxx) \;

    * z *= 2 \;
    (format t "xxx = ~A~%" xxx) \;

    return xxx \;
    })
  )
  

#|

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
