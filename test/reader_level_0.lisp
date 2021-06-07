(in-package #:with-c-syntax.test)

(in-readtable with-c-syntax-readtable)

(test test-reader-conservative
  ;; comma
  (is.equal.wcs 2
    #0{
    int hoge-array [ ] = { 0,1,2 } \;
    return hoge-array [ 2 ] \;
    }#)
  ;; ':'
  (is.equal.wcs 2
    #0{
    return 1 ? 2 : 3 \;
    }#)
  (is.equal.wcs :keyword
    #0{
    return NIL ? :never-comes-here : :keyword \;
    }#))

(test test-reader-conservative-default-level
  #.(setf *with-c-syntax-reader-level* 0)
  (is.equal.wcs 2
    #{
    return 1 ? 2 : 3 \;
    }#)
  #.(setf *with-c-syntax-reader-level* with-c-syntax.core::+with-c-syntax-default-reader-level+))
