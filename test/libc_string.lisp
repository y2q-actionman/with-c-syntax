(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(test test-string-strcpy
  #{
  register char * src = "src";
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

(test test-string-strchr
  ;; from cppreference;  https://en.cppreference.com/w/c/string/byte/strchr
  #{
  const char * str = "Try not. Do, or do not. There is no try.";
  char target = 'T';
  void * result = str;
  int output_count = 0;
  void * output;

  while((result = strchr(result, target)) != NULL) {
    output = with-output-to-string (`(*standard-output*), //; To preserve parens, I used Lisp escape
				     format(t, "Found '~C' starting at '~A'", target, result));

    switch (output_count) {
    case 0 :
      is (string= (output,
		   "Found 'T' starting at 'Try not. Do, or do not. There is no try.'"));
      break;
    case 1 :
      is (string= (output,
		   "Found 'T' starting at 'There is no try.'"));
      break;
    }

    ++ output_count;
    result = subseq (result, 1); // Proceed it. (in original code, incremented 'result' pointer).
  }
  }#
  ;; FIXME: If I use internal declaration (like 'void * output = ...'),
  ;; it is initialized only once!

  #{
  is (! strchr ("hoge", #\x));
  is (! strchr ("", #\x));
  is (equal ( strchr ("foobar", #\nul), ""));
  }#)

(test test-string-strrchr
  ;; https://en.cppreference.com/w/c/string/byte/strrchr
  #{
    char szSomeFileName [] = "foo/bar/foobar.txt"; // TODO: use 'szDomeFileName[]' style.
    char * pLastSlash = strrchr(szSomeFileName, '/');
    // ; I use `subseq' instead of pointer calculation.
    char * pszBaseName = pLastSlash ? subseq (pLastSlash, 1) : szSomeFileName;
    is (string= (format(nil, "Base Name: ~A", pszBaseName), // ; TODO: use `printf'.
		       "Base Name: foobar.txt"));
    }#

  #{
  is (! strrchr ("hoge", #\x));
  is (! strrchr ("", #\x));
  is (equal ( strrchr ("foobar", #\nul), ""));
  }#)

(test test-string-strspn
  ;; see https://en.cppreference.com/w/c/string/byte/strspn
  #{
    register const char low_alpha[] = "qwertyuiopasdfghjklzxcvbnm";

    is (strspn( "abcde312$#@", low_alpha) == 5);
    
    is (strspn( "!?", low_alpha) == 0);
    is (strspn( "", low_alpha) == 0);
    is (strspn( "a", low_alpha) == 1);
    is (strspn( "abc", low_alpha) == 3);
  }#)

(test test-string-strcspn
  ;; see https://en.cppreference.com/w/c/string/byte/strcspn
  #{
    const char * string = "abcde312$#@";
    const char * invalid = "*$#";
 
    is (strcspn( string, invalid) == 8);
    
    is (strcspn( "!?", invalid) == 2);
    is (strcspn( "", invalid) == 0);
    is (strcspn( "$$$", invalid) == 0);
    is (strcspn( "abcdef", invalid) == 6);
  }#)

(test test-string-strpbrk
  ;; see https://en.cppreference.com/w/c/string/byte/strpbrk
  #{
    const char * str = "hello world, friend of mine!";
    const char * sep = " ,!";
 
    unsigned int cnt = 0;
    do {
       str = strpbrk(str, sep);
       if(str)
       str = subseq (str, strspn(str, sep)); // I use `subseq' instead of pointer arithmetics.
       
       switch (cnt) {
       case 0 : is (string= (str, "world, friend of mine!")); break;
       case 1 : is (string= (str, "friend of mine!")); break;
       case 2 : is (string= (str, "of mine!")); break;
       case 3 : is (string= (str, "mine!")); break;
       case 4 : is (string= (str, "")); break;
       default : is (progn ("cnt must be < 5", nil));
       }

       ++ cnt;
     } while(str && ! alexandria:length= (str, 0));

     is (cnt == 5);
  }#

  #{
    const char * sep = "*$#";
 
    is (string= (strpbrk( "a#", sep), "#"));
    is (string= (strpbrk( "#a", sep), "#a"));
    is (string= (strpbrk( "", sep), nil));
    is (string= (strpbrk( "$$$", sep), "$$$"));
    is (string= (strpbrk( "abc", sep), nil));
  }#)


(test test-string-strstr
  ;; from BSD man page
  #{
  const char * largestring = "Foo Bar Baz";
  const char * smallstring = "Bar";
  char * ptr;

  ptr = strstr (largestring, smallstring);
  is (string= (ptr, "Bar Baz"));
  }#)					; TODO: increase tests!

(test test-string-strtok
  #{
    is (strtok ("", ",!?") == NULL);
    is (strtok ("!!!", ",!?") == NULL);
  }#

  #{
  is (string= (strtok ("aaa;;bbb,", ";,"), "aaa"));
  is (string= (strtok (NULL, ";,"), "bbb"));
  is (eql (strtok (NULL, ";,"), NULL));
  is (eql (strtok (NULL, ";,"), NULL));
  }#

  #{
  is (string= (strtok ("aaa;;bbb,", ""), "aaa;;bbb,"));
  is (eql (strtok (NULL, ""), NULL));
  }#

  ;; https://en.cppreference.com/w/c/string/byte/strtok
  (let ((input "A bird came down the walk")
	token-list)
    #{
      void * token = strtok(input, " ");
      while(token) {
        push (token, token-list);
        token = strtok(NULL, " ");
      }
      token-list = nreverse (token-list);
    }#
    ;; My 'strtok' implementation does not destroy the original string.
    ;; (this behavior is required??)
    (is (tree-equal token-list
		    '("A" "bird" "came" "down" "the" "walk")
		    :test #'string=)))
  t)


(test test-string-memchr
  #{
  void * v = `#(0 1 2 3 4);
  void * expected;

  expected = `#(0 1 2 3 4);
  is (equalp (memchr (v, 0, 5), expected));
  is (equalp (memchr (v, 0, 1), expected));
  is (equalp (memchr (v, 0, 15), expected));
  is (equalp (memchr (v, 0, 0), nil));

  expected = `#(2 3 4);
  is (equalp (memchr (v, 2, 5), expected));
  is (equalp (memchr (v, 2, 1), nil));
  is (equalp (memchr (v, 2, 15), expected));
  is (equalp (memchr (v, 2, 0), nil));

  is (equalp (memchr (v, 5, 5), nil));
  is (equalp (memchr (v, 5, 1), nil));
  is (equalp (memchr (v, 5, 15), nil));
  is (equalp (memchr (v, 5, 0), nil));
  }#)

(test test-string-memcmp
  #{
  is (memcmp (`#(), `#(), 1) == 0);
  is (memcmp (`#(1), `#(1), 1) == 0);
  is (memcmp (`#(1), `#(1), 0) == 0);

  is (memcmp (`#(0 0), `#(0 1), 2) < 0);
  is (memcmp (`#(0 0), `#(1 0), 2) < 0);
  is (memcmp (`#(1 0), `#(0 1), 2) > 0);
  is (memcmp (`#(1 0), `#(1 1), 2) < 0);
  is (memcmp (`#(1 0 1), `#(1 1), 2) < 0);
  is (memcmp (`#(1 0 1), `#(1 1 1), 2) < 0);

  is (memcmp ("abc", "abd", 3, :predicate, #'char<) < 0);
  is (memcmp ("abd", "abc", 3, :predicate, #'char<) > 0);
  is (memcmp ("abc", "abc", 3, :predicate, #'char<) == 0);
  }#)

(test test-string-memset
  #{
  void * v = `#(0 1 2 3 4);

  is (equalp (memset (v, 0, 2), `#(0 0 2 3 4)));
  is (equalp (v, `#(0 0 2 3 4)));

  is (equalp (memset (v, #x10, 5), `#(#x10 #x10 #x10 #x10 #x10)));
  is (equalp (v, `#(#x10 #x10 #x10 #x10 #x10)));
  }#)

(test test-string-memcpy
  #{
  void * v = `#(0 1 2 3 4);
  void * w = `#(99 98 97);

  is (equalp (memcpy (v, w, 2), `#(99 98 2 3 4)));
  is (equalp (v, `#(99 98 2 3 4)));

  is (equalp (memcpy (v, w, 3), `#(99 98 97 3 4)));
  is (equalp (v, `#(99 98 97 3 4)));
  }#)

(test test-string-memmove
  #{
  void * v = `#(0 1 2 3 4);
  void * w = `#(99 98 97);

  is (equalp (memmove (v, w, 2), `#(99 98 2 3 4)));
  is (equalp (v, `#(99 98 2 3 4)));

  is (equalp (memmove (v, w, 3), `#(99 98 97 3 4)));
  is (equalp (v, `#(99 98 97 3 4)));
  }#)
