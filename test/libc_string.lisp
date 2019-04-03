(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(test test-string-strcpy
  #{
  // ; char * register src = "src"; // FIXME
  char * src = "abcde";
  // ; register char dst [ 10 ] = "abcde"; // FIXME
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
  void * dst; // TODO: add more 'void*' tests.

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

(test test-string-strcat
  #{
  void * dst;
  char * src = "abcde";
  char * prefix = "XYZ";
  char * expected = concatenate (quote (string), prefix, src);

  // ; normal concatenatation.
  dst = copy-seq (prefix) ;
  dst = strcat (dst, src);
  is (string= (dst, expected));

  // ; dst has fill-pointer and enough storage.
  dst = make-array(10, :element-type, quote (character),
		       :fill-pointer, 3);
  replace (dst, prefix);
  dst = strcat (dst, src);
  is (string= (dst, expected));

  // ; dst has fill-pointer but storage is short.
  dst = make-array(4, :element-type, quote (character),
		      :fill-pointer, 3);
  replace (dst, prefix);
  dst = strcat (dst, src);
  is (string= (dst, expected));
  }#)

(test test-string-strncat
  #{
  void * dst;
  char * src = "abcde";
  char * prefix = "XYZ";

  // ; normal concatenatation.
  dst = copy-seq (prefix) ;
  dst = strncat (dst, src, 0);
  is (string= (dst, "XYZ"));

  dst = strncat (dst, src, 3);
  is (string= (dst, "XYZabc"));

  dst = strncat (dst, src, 7);
  is (string= (dst, "XYZabcabcde"));

  }#)

(test test-string-strlen
  #{
  is (strlen ("") == 0);
  is (strlen ("a") == 1);
  is (strlen ("0123456789") == 10);
  }#)

(test test-string-strcmp
  #{
  is (zerop (strcmp ("ABC", "ABC")));
  is (minusp (strcmp ("ABC", "ABD")));
  is (minusp (strcmp ("ABC", "B")));
  is (plusp (strcmp ("ABC", "AAAA")));

  is (zerop (strcmp ("abc", "abc")));
  is (plusp (strcmp ("abc", "ZZZ")));
  is (minusp (strcmp ("abc", "abcd")));
  }#)

(test test-string-strncmp
  #{
  is (zerop (strncmp ("ABC", "ABD", 2)));
  is (minusp (strncmp ("ABC", "ABD", 3)));
  is (minusp (strncmp ("ABC", "ABD", 4)));
  
  is (zerop (strncmp ("ABC", "B", 0)));
  is (minusp (strncmp ("ABC", "B", 1)));
  is (minusp (strncmp ("ABC", "B", 2)));

  is (zerop (strncmp ("ABC", "AAAA", 1)));
  is (plusp (strncmp ("ABC", "AAAA", 2)));
  is (plusp (strncmp ("ABC", "AAAA", 3)));

  is (zerop (strncmp ("abc", "abcd", 3)));
  is (minusp (strncmp ("abc", "abcd", 4)));
  is (minusp (strncmp ("abc", "abcd", 5)));
  }#)
