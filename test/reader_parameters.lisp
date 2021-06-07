(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

#.(setf *with-c-syntax-reader-level* 2)
#0{
int test-reader-toplevel-conservative \( \) {
  return t \;
}
}#

#.(setf *with-c-syntax-reader-level* 0)
#1{
int test-reader-toplevel-aggressive(){
  int hoge-array[]={0,1,2};
  return hoge-array[2]== 2;
}
}#

#2{
int test\-reader\-toplevel\-overkill(){
  assert (1+2*3-4 == `(+ 1 (* 2 3) (- 4)));
  return t;
}
}#

(test test-toplevel-reader
  (is (test-reader-toplevel-conservative))
  (is (test-reader-toplevel-aggressive))
  (is (test-reader-toplevel-overkill)))

#.(setf *with-c-syntax-reader-level* with-c-syntax.core::+with-c-syntax-default-reader-level+)
#.(setf *with-c-syntax-reader-case* :preserve)
(test test-reader-case-sensitivity
  (is.equal.wcs nil
    #{
    int x \, X \;
    x = 1 \;
    X = 2 \;
    return x == X \;
    }#))

;;; Agh, I need file-local variable..
#.(setf *with-c-syntax-reader-level* with-c-syntax.core::+with-c-syntax-default-reader-level+)
#.(setf *with-c-syntax-reader-case* nil)
