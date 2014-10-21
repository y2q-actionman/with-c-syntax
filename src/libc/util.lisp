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

(defun define-preprocessor-macro-with-upcase (name val)
  (define-preprocessor-macro name val)
  (define-preprocessor-macro (string-upcase name) val :upcase))

(defun define-predefined-typedef-and-aliases (sym type)
  (add-typedef sym type)		; typedefs name -> type
  ;; addes package-free alias
  (define-preprocessor-macro-with-upcase (symbol-name sym) sym))
