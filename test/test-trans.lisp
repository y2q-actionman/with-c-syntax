(in-package :with-c-syntax)

;;; translation-unit

(defun test-trans-decl ()
  (test '( int a \; ))
  (test '( int a \; int b \; ))
  t)

(defun test-trans-fdefinition ()
  (test '( int hoge \( x \, y \) int x \, y \; { }  ))

  (test '( hoge \( x \, y \) int x \, y \; { }  ))

  (test '( int hoge \( \) { }  ))
  (test '( int hoge \( x \) { }  ))
  (test '( hoge \( \) { }  ))
  (test '( hoge \( x \) { }  ))
  (test '( hoge \( int x \, float y \) { }  ))
  t)

