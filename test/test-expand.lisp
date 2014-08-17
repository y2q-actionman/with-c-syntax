(in-package :with-c-syntax)

(defun test-hello-world ()
  (with-c-syntax ()
    {
    format \( t \, "Hello, World!~%" \) \;
    })
  )

(defun test-add-const ()
  (with-c-syntax ()
    {
    return 1 + 2 \;
    })
  )
;; => 3

(defparameter x 0)

(defun test-while-loop ()
  (with-c-syntax ((x x))                ; closes 'x'
    {
    while \( x < 100 \)
      x ++ \;
    return x \;
    })
  )
;; => 100

(defun test-for-loop ()
  (let ((i 0) (sum 0))
    (with-c-syntax ()
      {
      for \( i = 0 \; i < 100 \; ++ i \)
         sum += i \;
      })
    sum))
;; => 5050

(defun test-loop-continue-break ()
  (let ((i 0) (sum 0))
    (with-c-syntax ()
      {
      for \( i = 0 \; i < 100 \; ++ i \) {
        if \( (oddp i) \)
          continue \;
        if \( i == 50 \)
          break \;
        sum += i \;
        (format t "i ~A, sum ~A~%" i sum) \;
      }
      })
    sum))
;; => 600

(defun test-switch ()
  (flet ((fun (x)
           (with-c-syntax ((x x))
    {
      format \( t \, "[~A] " \, x \) \;
      switch \( x \) {
      case 1 \:
        (format t "case 1~%") \;
        break \;
      case 2 \:
        (format t "case 2~%") \;
        (format t "fall-though 2->3~%") \;
      case 3 \:
        (format t "case 3~%") \;
        break \;
      case 4 \:
        (format t "case 4~%") \;
        break \;
      default \:
        (format t "default~%") \;
      }
    })))
    (loop for i from 0 to 5
       do (fun i))))
#|
CL-USER> (test-switch)
[0] default
[1] case 1
[2] case 2
fall-though 2->3
case 3
[3] case 3
[4] case 4
[5] default
|#

(defun test-goto ()
  (with-c-syntax ()
    {
      goto d \;

    a \:
      princ \( "a" \) \;

    b \:
      princ \( "b" \) \;
      goto e \;

    c \:
      princ \( "c" \) \;
      return \;

    d \:
      princ \( "d" \) \;
      goto a \;  
    
    e \:
      princ \( "e" \) \;
      goto c \;
    })
  )
#|
CL-USER> (test-goto)
dabec
; No value
|#

(defun test-pointer (xxx &aux z)
  (with-c-syntax ((xxx xxx))
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
CL-USER> (test-pointer 10)
z = 32
xxx = 10
xxx = 11
xxx = 22
22
|#
  
(defun test-duff-device (to-seq from-seq cnt)
  (with-c-syntax ((to-seq to-seq) (from-seq from-seq) (cnt cnt)
                  to from n)
    {
    to = & to-seq \;
    from = & from-seq \;

    n = \( cnt + 7 \) / 8 \;
    n = floor \( n \) \;                ; Lisp's CL:/ produces rational
    switch \( cnt % 8 \) {
    case 0 \:	do {	* to ++ = * from ++ \;
    case 7 \:		* to ++ = * from ++ \;
    case 6 \:		* to ++ = * from ++ \;
    case 5 \:		* to ++ = * from ++ \;
    case 4 \:		* to ++ = * from ++ \;
    case 3 \:		* to ++ = * from ++ \;
    case 2 \:		* to ++ = * from ++ \;
    case 1 \:		* to ++ = * from ++ \;
      } while \( -- n > 0 \) \;
    }
    })
  to-seq)
#|
CL-USER> (setf arr1 (make-array 20 :initial-element 1))
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
CL-USER> (setf arr2 (make-array 20 :initial-element 2))
#(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
CL-USER> (test-duff-device arr1 arr2 10)
mod 2
n = 17/8
n = 2
#(2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1)
CL-USER> arr1
#(2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1)
|#
