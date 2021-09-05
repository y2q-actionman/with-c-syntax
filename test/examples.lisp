(in-package #:with-c-syntax.test)

(defun wcs-hello-world ()
  (with-c-syntax ()
    format \( t \, "Hello, World!" \) \;
    ))

(test test-hello-world
  (is (string= "Hello, World!"
	       (with-output-to-string (*standard-output*)
		 (wcs-hello-world)))))

(defun wcs-add-const ()
  (with-c-syntax ()
    return 1 + 2 \;
    ))

(test test-add-const
  (is (= 3 (wcs-add-const))))

(defun wcs-add-args (x y)
  (with-c-syntax ()
    return x + y \;
    ))

(test test-add-args
  (is (= 3 (wcs-add-args 1 2))))

(defun wcs-while-loop (&aux (x 0))
  (with-c-syntax ()
    while \( x < 100 \)
      x ++ \;
    return x \;
    ))

(test test-while-loop
  (is (= 100 (wcs-while-loop))))
  
(defun wcs-for-loop ()
  (let ((i 0) (sum 0))
    (with-c-syntax ()
      for \( i = 0 \; i <= 100 \; ++ i \)
         sum += i \;
      )
    sum))

(test test-for-loop
  (is (= 5050 (wcs-for-loop))))

(defun wcs-loop-continue-break (&aux (i 0) (sum 0))
  (with-c-syntax ()
    for \( i = 0 \; i < 100 \; ++ i \) {
      if \( (oddp i) \)
        continue \;
      if \( i == 50 \)
        break \;
      sum += i \;
    }
    return sum \;
   ))

(test test-loop-continue-break
  (is (= 600 (wcs-loop-continue-break))))

(defun wcs-switch (x &aux (ret nil))
  (with-c-syntax ()
    switch \( x \) {
    case 1 \:
      (push 'case1 ret) \;
      break \;
    case 2 \:
      (push 'case2 ret) \;
      ;; fall-though
    case 3 \:
      (push 'case3 ret) \;
      break \;
    case 4 \:
      (push 'case4 ret) \;
      break \;
    default \:
      (push 'default ret) \;
    })
  (nreverse ret))

(test test-switch
  (is (equal '(case1) (wcs-switch 1)))
  (is (equal '(case2 case3) (wcs-switch 2)))
  (is (equal '(case3) (wcs-switch 3)))
  (is (equal '(case4) (wcs-switch 4)))
  (is (equal '(default) (wcs-switch 5))))

(defun wcs-goto (&aux (ret nil))
  (with-c-syntax ()
      goto d \;

    a \:
      push \( 'a \, ret \) \;

    b \:
      push \( 'b \, ret \) \;
      goto e \;

    c \:
      push \( 'c \, ret \) \;
      return nreverse \( ret \) \;

    d \:
      push \( 'd \, ret \) \;
      goto a \;  
    
    e \:
      push \( 'e \, ret \) \;
      goto c \;
    ))

(test test-goto
  (is (equal '(d a b e c) (wcs-goto))))

(defun wcs-pointer (xxx &aux z)
  (with-c-syntax ()
    z =  & xxx \;
    * z += 1 \;
    * z *= 2 \;
    return xxx \;
    ))

(test test-pointer
  (is (= 2 (wcs-pointer 0)))
  (is (= 4 (wcs-pointer 1)))
  (is (= 22 (wcs-pointer 10))))

(in-readtable with-c-syntax-readtable)  
(defun wcs-duff-device (to-seq from-seq cnt)
  (with-c-syntax ()
    #{
    int *to = &to-seq;
    int *from = &from-seq;

    int n = (cnt + 7) / 8;
    n = floor(n);           #| Lisp's CL:/ produces rational |#
    switch (cnt % 8) {
    case 0 :	do {	*to++ = *from++;
    case 7 :		*to++ = *from++;
    case 6 :		*to++ = *from++;
    case 5 :		*to++ = *from++;
    case 4 :		*to++ = *from++;
    case 3 :		*to++ = *from++;
    case 2 :		*to++ = *from++;
    case 1 :		*to++ = *from++;
      } while (--n > 0);
    }
    }#)
  to-seq)

#{
int *array-2* [] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2};
}#

(test test-duff-device ()
  (let ((arr1 (make-array 20 :initial-element 1))
	(arr2 (locally (declare (special *array-2*))
                (copy-seq *array-2*))))
    (wcs-duff-device arr1 arr2 10)
    (is (equalp #(2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1)
		arr1))))

(test test-auto-return
  (is (= 100 (with-c-syntax (:return :auto)
	       100 \;)))
  (is (= 1 (with-c-syntax (:return :auto)
	     int i = 1 \; i \;)))
  (is (= 3 (with-c-syntax (:return :auto)
	     1 \; 2 \; 3 \;)))
  
  ;; can be expanded, but does not works (`return' or other ways required.)
  (is (= 4 (with-c-syntax (:return :auto)
	     if \( 10 \) return 4 \;)))
  (is (= 5 (with-c-syntax (:return :auto)
	     ;; FIXME
	     ;; `nil' cannot be directly used, we must quote it..
	     if \( 'nil \) 4 \; else return 5 \;)))
  (is (eq (readtable-case *readtable*)
	  (with-c-syntax (:return :auto)
	    switch \( (readtable-case *readtable*) \) {
	    case :upcase \: return :upcase \;
	    case :downcase \: return :downcase \;
	    case :preserve \: return :preserve \;
	    case :invert \: return :invert \;
	    default \: return "unknown!" \;
	    })))
  (let ((tmp 0))
    (with-c-syntax (:return :auto)
      int i \;
      for \( i = 0 \; i <= 100 \; ++ i \)
      tmp += i \;
      )
    (is (= tmp 5050)))
  (is (= 5050
	 (with-c-syntax (:return :auto)
	   int i = 0 \, j = 0 \;
	   next_loop \:
	   if \( i > 100 \) return j \;
	   j += i \;
	   ++ i \;
	   goto next_loop \;))))

;;; from 2018-12 new README.

(test test-sum-1-100
  (is (= 5050
	 ;; This curious indent avoids syntactic confusion of my editor.
	 #{
	 int i, sum = 0;

	 for (i = 0;
		i <= 100;
		++i )
		sum += i;
	 return sum;
	 }#
	 )))

(defun array-transpose (arr)
  (destructuring-bind (i-max j-max) (array-dimensions arr)
    #{
      int i,j;
      for (i = 0;
	     i < i-max;
	     i++) {
             for (j = i + 1;
		    j < j-max;
		    j++) {
	      rotatef(arr[i][j], arr[j][i]);
              // Thanks of CL, I can use `rotatef' for swapping.
	      // I've wrote this code here:
              //   temp = arr[i][j];
              //   arr[i][j] = arr[j][i];
              //   arr[j][i] = temp;
          }
      }
    }#)
  arr)

(test test-array-transpose
  (is (equalp (array-transpose
	       (make-array '(3 3)
 			   :initial-contents '((0 1 2) (3 4 5) (6 7 8))))
	      #2A((0 3 6) (1 4 7) (2 5 8)))))

#{
int sum-of-list (list) {
  int list-length = length(list);
  int i, ret = 0;

  for (i = 0;
	 i < list-length;
	 ++i) {
     ret += nth(i, list);
  }

  return ret;
}
}#

(test test-sum-of-list
  (is (eql 55
	   (sum-of-list '(1 2 3 4 5 6 7 8 9 10)))))


;; http://www.99-bottles-of-beer.net/language-c-844.html
#{
void 99-bottles-of-beer (filename) {
  void * output-path = merge-pathnames (filename, user-homedir-pathname());
  `(with-open-file (*standard-output* output-path :direction :output
				      :if-exists :supersede :if-does-not-exist :create)
     #{
     int b;
     for (b = 99;
	    b >= 0;
	    b -- ) {
	    switch (b) {
            case 0 :
              write-line("No more bottles of beer on the wall, no more bottles of beer.");
              write-line("Go to the store and buy some more, 99 bottles of beer on the wall.");
              break;
            case 1 :
              write-line("1 bottle of beer on the wall, 1 bottle of beer.");
              write-line("Take one down and pass it around, no more bottles of beer on the wall.");
              break;
            default :
              format(t, "~D bottles of beer on the wall, ~D bottles of beer.~%", b, b);      
              format(t, "Take one down and pass it around, ~D ~A of beer on the wall.~%"
                        , b - 1
                          , ((b - 1) > 1)? "bottles" : "bottle");
              break;
            }
	    }
      }#);
  return;
  }
}#

(defparameter *test-file-name*
  (make-pathname :name "bottles_of_beer" :type "txt"))

(test test-c-lisp-c-lisp
  (let ((output-path (merge-pathnames *test-file-name*
				      (user-homedir-pathname))))
    (unwind-protect
	 (progn
	   (when (probe-file output-path)
	     (delete-file output-path))
	   (99-bottles-of-beer *test-file-name*)
	   (is (probe-file output-path))
	   (with-open-file (s output-path)
	     ;; 99 bottles of beer -> 200 Lines
	     (= 200 (loop for l = (read-line s nil nil)
		       while l count l))
	     ;; In Mac OS X, 11786 bytes. It is 'LF', not CR LF.
	     (let ((mac-os-x-99b-file-size 11786))
	       (is (<= mac-os-x-99b-file-size
		       (file-length s)
		       (+ mac-os-x-99b-file-size 200))))))
      (when (probe-file output-path)
	(delete-file output-path)))))

(test test-readme-inline-usage
  (is (null (assert (= 100 #{ 98 - 76 + 54 + 3 + 21 }#))))
  (is (null (assert #2{ 1+2+3-4+5+6+78+9 == 100 }#)))
  (let ((*standard-output* (make-broadcast-stream)))
    (is (princ #{ 0x1.fffp+1 }#))))

#{
#define MY_MAX(x, y) ((x)>(y) ? (x) : (y))

int my-max-test (x, y) {
  return MY_MAX (x, y);
}
}#

#{
#define MY_CL_MAX(x, ...) cl:max(x, __VA_ARGS__)

int my-cl-max-test (x, y, z) {
  return MY_CL_MAX (x, y, z);
}
}#

(test test-readme-define-usage
  (is (= (my-max-test -1 1) 1))
  (is (= (my-cl-max-test -1 9999 1) 9999))
  (string=
   "1.2"
   #2{
   #define STR(x) #x
   #define EXPAND_STR(x) STR(x) 
   #define CAT(x,y) x##y
   EXPAND_STR(CAT(1,.2))
   }#))

#{
#define TEST_MACRO_DEFINITION

int test-macro-defined-p () {
#ifdef TEST_MACRO_DEFINITION
  return t;
#else
  return nil;
#endif
}
}#

(defun see-features-example ()
  #{
  #if `(member :sbcl *features* :test 'eq)
  format(nil, "I am SBCL: ~A", lisp-implementation-version());
  #elif `(member :allegro *features* :test 'eq)
  format(nil, "I am ALLEGRO: ~A", lisp-implementation-version());
  #else
  "Under implementation";
  #endif
  }#)

(test test-readme-conditional-inclusion
  (is (test-macro-defined-p))
  (is (equal
       (see-features-example)
       #+sbcl (format nil "I am SBCL: ~A" (lisp-implementation-version))
       #+allegro (format nil "I am ALLEGRO: ~A" (lisp-implementation-version))
       #-(or sbcl allegro) "Under implementation"
       )))

;;; This example is very difficult to run. Making a file to be
;;; compiled in the compilation time causes many compile errors..
(eval-when (:execute)
  (with-making-include-file (stream "/tmp/tmp.h")
      (format stream "const int foo = 100;")
    (defun return-foo ()
      #{
      #include "/tmp/tmp.h"
      return foo;
      }#))

  (with-making-include-file (stream "/tmp/tmp.h")
      (format stream "const int bar = 123;")
    (defpackage temp-package
      (:use :cl)
      (:export #:bar))
    #2{
    _Pragma("WITH_C_SYNTAX IN_PACKAGE \"TEMP-PACKAGE\"")
    #include "/tmp/tmp.h"
    }#)

  (test test-readme-include
    (is (eql (return-foo) 100))
    (is (eql temp-package:bar 123))))
