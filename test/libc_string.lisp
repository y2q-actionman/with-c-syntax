(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(test test-string-strcpy
  #{
  // char * register src = "src"; // FIXME
  char * src = "abcde";
  // register char dst [ 10 ] = "abcde"; // FIXME
  register dst-same = `(make-string 5);
  register dst-shorter = `(make-string 3);
  register dst-longer = `(make-string 10);

  dst-same = strcpy (dst-same, src);
  is (string= (dst-same, src));

  dst-shorter = strcpy (dst-shorter, src);
  is (string= (dst-shorter, src, :end1, length (dst-shorter)));

  dst-longer = strcpy (dst-longer, src);
  is (string= (dst-longer, src, :end1, length (src)));
  }#)

(test test-string-strncpy
  #{
  char * src = "abcde";
  char * dst; // FIXME: support void*.

  dst = strncpy (make-string (5), src, 5);
  is (string= (dst, src));

  dst = strncpy (make-string (5), src, 3);
  is (string/= (dst, src));
  is (string= (dst, src, :end2, 3));

  dst = strncpy (make-string (5), src, 7);
  is (string= (dst, src, :end1, 5));

  dst = strncpy (make-string (5), src, 4);
  is (string= (dst, src, :end1, 4, :end2, 4));

  dst = strncpy (make-string (7), src, 7);
  is (string= (dst, src, :end1, 5, :end2, 5));
  is (char= (code-char (0), dst [5], dst [6]));
  }#)
