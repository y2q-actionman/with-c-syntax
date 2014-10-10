(in-package #:with-c-syntax.stdlib)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun mantissa-radix-change (mantissa from-radix to-radix)
  (+ (floor (* (1- mantissa)
               (log from-radix to-radix)))
     (if (= from-radix to-radix) 1 0)))

(defun signed-byte-max (bits)
  (1- (expt 2 (1- bits))))

(defun signed-byte-min (bits)
  (- (expt 2 (1- bits))))

(defun unsigned-byte-max (bits)
  (1- (expt 2 bits))))

(defun define-predefined-typedef-and-aliases (name type aliases)
  ;; typedefs name -> type
  (define-predefined-typedef name type)
  ;; addes package-free alias
  (loop for i in aliases
       do (define-preprocessor-symbol i name)))
