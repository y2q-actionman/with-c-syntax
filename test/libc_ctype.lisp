(in-package #:with-c-syntax.test)

;;; I decided to write libc tests with `with-c-syntax' itself,
;;; if they are not free-standing.
(in-readtable with-c-syntax-readtable)

(test test-ctype-isalnum
  #2{
  // control
  is(! isalnum('\x00'));
  is(! isalnum('\x08'));
  // tab
  is(! isalnum('\x09'));
  // whitespaces
  is(! isalnum('\x0A'));
  is(! isalnum('\x0D'));
  // control
  is(! isalnum('\x0E'));
  is(! isalnum('\x1F'));
  // space
  is(! isalnum('\x20'));
  // symbols
  is(! isalnum('\x21'));
  is(! isalnum('\x2F'));
  // 0-9
  is(isalnum('\x30'));
  is(isalnum('\x39'));
  // symbols
  is(! isalnum('\x3A'));
  is(! isalnum('\x40'));
  // A-F
  is(isalnum('\x41'));
  is(isalnum('\x46'));
  // G-Z
  is(isalnum('\x47'));
  is(isalnum('\x5A'));
  // symbols
  is(! isalnum('\x5B'));
  is(! isalnum('\x60'));
  // a-f
  is(isalnum('\x61'));
  is(isalnum('\x66'));
  // g-z
  is(isalnum('\x67'));
  is(isalnum('\x7A'));
  // symbols
  is(! isalnum('\x7B'));
  is(! isalnum('\x7E'));
  // rubout
  is(! isalnum('\x7F'));
  }#)

(test test-ctype-isalpha
  #2{
  // control
  is(! isalpha('\x00'));
  is(! isalpha('\x08'));
  // tab
  is(! isalpha('\x09'));
  // whitespaces
  is(! isalpha('\x0A'));
  is(! isalpha('\x0D'));
  // control
  is(! isalpha('\x0E'));
  is(! isalpha('\x1F'));
  // space
  is(! isalpha('\x20'));
  // symbols
  is(! isalpha('\x21'));
  is(! isalpha('\x2F'));
  // 0-9
  is(! isalpha('\x30'));
  is(! isalpha('\x39'));
  // symbols
  is(! isalpha('\x3A'));
  is(! isalpha('\x40'));
  // A-F
  is(isalpha('\x41'));
  is(isalpha('\x46'));
  // G-Z
  is(isalpha('\x47'));
  is(isalpha('\x5A'));
  // symbols
  is(! isalpha('\x5B'));
  is(! isalpha('\x60'));
  // a-f
  is(isalpha('\x61'));
  is(isalpha('\x66'));
  // g-z
  is(isalpha('\x67'));
  is(isalpha('\x7A'));
  // symbols
  is(! isalpha('\x7B'));
  is(! isalpha('\x7E'));
  // rubout
  is(! isalpha('\x7F'));
  }#)

(test test-ctype-islower
  #2{
  // control
  is(! islower('\x00'));
  is(! islower('\x08'));
  // tab
  is(! islower('\x09'));
  // whitespaces
  is(! islower('\x0A'));
  is(! islower('\x0D'));
  // control
  is(! islower('\x0E'));
  is(! islower('\x1F'));
  // space
  is(! islower('\x20'));
  // symbols
  is(! islower('\x21'));
  is(! islower('\x2F'));
  // 0-9
  is(! islower('\x30'));
  is(! islower('\x39'));
  // symbols
  is(! islower('\x3A'));
  is(! islower('\x40'));
  // A-F
  is(! islower('\x41'));
  is(! islower('\x46'));
  // G-Z
  is(! islower('\x47'));
  is(! islower('\x5A'));
  // symbols
  is(! islower('\x5B'));
  is(! islower('\x60'));
  // a-f
  is(islower('\x61'));
  is(islower('\x66'));
  // g-z
  is(islower('\x67'));
  is(islower('\x7A'));
  // symbols
  is(! islower('\x7B'));
  is(! islower('\x7E'));
  // rubout
  is(! islower('\x7F'));
  }#)

(test test-ctype-isupper
  #2{
  // control
  is(! isupper('\x00'));
  is(! isupper('\x08'));
  // tab
  is(! isupper('\x09'));
  // whitespaces
  is(! isupper('\x0A'));
  is(! isupper('\x0D'));
  // control
  is(! isupper('\x0E'));
  is(! isupper('\x1F'));
  // space
  is(! isupper('\x20'));
  // symbols
  is(! isupper('\x21'));
  is(! isupper('\x2F'));
  // 0-9
  is(! isupper('\x30'));
  is(! isupper('\x39'));
  // symbols
  is(! isupper('\x3A'));
  is(! isupper('\x40'));
  // A-F
  is(isupper('\x41'));
  is(isupper('\x46'));
  // G-Z
  is(isupper('\x47'));
  is(isupper('\x5A'));
  // symbols
  is(! isupper('\x5B'));
  is(! isupper('\x60'));
  // a-f
  is(! isupper('\x61'));
  is(! isupper('\x66'));
  // g-z
  is(! isupper('\x67'));
  is(! isupper('\x7A'));
  // symbols
  is(! isupper('\x7B'));
  is(! isupper('\x7E'));
  // rubout
  is(! isupper('\x7F'));
  }#)

(test test-ctype-isdigit
  #2{
  // control
  is(! isdigit('\x00'));
  is(! isdigit('\x08'));
  // tab
  is(! isdigit('\x09'));
  // whitespaces
  is(! isdigit('\x0A'));
  is(! isdigit('\x0D'));
  // control
  is(! isdigit('\x0E'));
  is(! isdigit('\x1F'));
  // space
  is(! isdigit('\x20'));
  // symbols
  is(! isdigit('\x21'));
  is(! isdigit('\x2F'));
  // 0-9
  is(isdigit('\x30'));
  is(isdigit('\x39'));
  // symbols
  is(! isdigit('\x3A'));
  is(! isdigit('\x40'));
  // A-F
  is(! isdigit('\x41'));
  is(! isdigit('\x46'));
  // G-Z
  is(! isdigit('\x47'));
  is(! isdigit('\x5A'));
  // symbols
  is(! isdigit('\x5B'));
  is(! isdigit('\x60'));
  // a-f
  is(! isdigit('\x61'));
  is(! isdigit('\x66'));
  // g-z
  is(! isdigit('\x67'));
  is(! isdigit('\x7A'));
  // symbols
  is(! isdigit('\x7B'));
  is(! isdigit('\x7E'));
  // rubout
  is(! isdigit('\x7F'));
  }#)

(test test-ctype-isxdigit
  #2{
  // control
  is(! isxdigit('\x00'));
  is(! isxdigit('\x08'));
  // tab
  is(! isxdigit('\x09'));
  // whitespaces
  is(! isxdigit('\x0A'));
  is(! isxdigit('\x0D'));
  // control
  is(! isxdigit('\x0E'));
  is(! isxdigit('\x1F'));
  // space
  is(! isxdigit('\x20'));
  // symbols
  is(! isxdigit('\x21'));
  is(! isxdigit('\x2F'));
  // 0-9
  is(isxdigit('\x30'));
  is(isxdigit('\x39'));
  // symbols
  is(! isxdigit('\x3A'));
  is(! isxdigit('\x40'));
  // A-F
  is(isxdigit('\x41'));
  is(isxdigit('\x46'));
  // G-Z
  is(! isxdigit('\x47'));
  is(! isxdigit('\x5A'));
  // symbols
  is(! isxdigit('\x5B'));
  is(! isxdigit('\x60'));
  // a-f
  is(isxdigit('\x61'));
  is(isxdigit('\x66'));
  // g-z
  is(! isxdigit('\x67'));
  is(! isxdigit('\x7A'));
  // symbols
  is(! isxdigit('\x7B'));
  is(! isxdigit('\x7E'));
  // rubout
  is(! isxdigit('\x7F'));
  }#)

(test test-ctype-iscntrl
  #2{
  // control
  is(iscntrl('\x00'));
  is(iscntrl('\x08'));
  // tab
  is(iscntrl('\x09'));
  // whitespaces
  is(iscntrl('\x0A'));
  is(iscntrl('\x0D'));
  // control
  is(iscntrl('\x0E'));
  is(iscntrl('\x1F'));
  // space
  is(! iscntrl('\x20'));
  // symbols
  is(! iscntrl('\x21'));
  is(! iscntrl('\x2F'));
  // 0-9
  is(! iscntrl('\x30'));
  is(! iscntrl('\x39'));
  // symbols
  is(! iscntrl('\x3A'));
  is(! iscntrl('\x40'));
  // A-F
  is(! iscntrl('\x41'));
  is(! iscntrl('\x46'));
  // G-Z
  is(! iscntrl('\x47'));
  is(! iscntrl('\x5A'));
  // symbols
  is(! iscntrl('\x5B'));
  is(! iscntrl('\x60'));
  // a-f
  is(! iscntrl('\x61'));
  is(! iscntrl('\x66'));
  // g-z
  is(! iscntrl('\x67'));
  is(! iscntrl('\x7A'));
  // symbols
  is(! iscntrl('\x7B'));
  is(! iscntrl('\x7E'));
  // rubout
  is(iscntrl('\x7F'));
  }#)

(test test-ctype-isgraph
  #2{
  // control
  is(! isgraph('\x00'));
  is(! isgraph('\x08'));
  // tab
  is(! isgraph('\x09'));
  // whitespaces
  is(! isgraph('\x0A'));
  is(! isgraph('\x0D'));
  // control
  is(! isgraph('\x0E'));
  is(! isgraph('\x1F'));
  // space
  is(! isgraph('\x20'));
  // symbols
  is(isgraph('\x21'));
  is(isgraph('\x2F'));
  // 0-9
  is(isgraph('\x30'));
  is(isgraph('\x39'));
  // symbols
  is(isgraph('\x3A'));
  is(isgraph('\x40'));
  // A-F
  is(isgraph('\x41'));
  is(isgraph('\x46'));
  // G-Z
  is(isgraph('\x47'));
  is(isgraph('\x5A'));
  // symbols
  is(isgraph('\x5B'));
  is(isgraph('\x60'));
  // a-f
  is(isgraph('\x61'));
  is(isgraph('\x66'));
  // g-z
  is(isgraph('\x67'));
  is(isgraph('\x7A'));
  // symbols
  is(isgraph('\x7B'));
  is(isgraph('\x7E'));
  // rubout
  is(! isgraph('\x7F'));
  }#)

(test test-ctype-isspace
  #2{
  // control
  is(! isspace('\x00'));
  is(! isspace('\x08'));
  // tab
  is(isspace('\x09'));
  // whitespaces
  is(isspace('\x0A'));
  is(isspace('\x0D'));
  // control
  is(! isspace('\x0E'));
  is(! isspace('\x1F'));
  // space
  is(isspace('\x20'));
  // symbols
  is(! isspace('\x21'));
  is(! isspace('\x2F'));
  // 0-9
  is(! isspace('\x30'));
  is(! isspace('\x39'));
  // symbols
  is(! isspace('\x3A'));
  is(! isspace('\x40'));
  // A-F
  is(! isspace('\x41'));
  is(! isspace('\x46'));
  // G-Z
  is(! isspace('\x47'));
  is(! isspace('\x5A'));
  // symbols
  is(! isspace('\x5B'));
  is(! isspace('\x60'));
  // a-f
  is(! isspace('\x61'));
  is(! isspace('\x66'));
  // g-z
  is(! isspace('\x67'));
  is(! isspace('\x7A'));
  // symbols
  is(! isspace('\x7B'));
  is(! isspace('\x7E'));
  // rubout
  is(! isspace('\x7F'));
  }#)

(test test-ctype-isblank
  #2{
  // control
  is(! isblank('\x00'));
  is(! isblank('\x08'));
  // tab
  is(isblank('\x09'));
  // whitespaces
  is(! isblank('\x0A'));
  is(! isblank('\x0D'));
  // control
  is(! isblank('\x0E'));
  is(! isblank('\x1F'));
  // space
  is(isblank('\x20'));
  // symbols
  is(! isblank('\x21'));
  is(! isblank('\x2F'));
  // 0-9
  is(! isblank('\x30'));
  is(! isblank('\x39'));
  // symbols
  is(! isblank('\x3A'));
  is(! isblank('\x40'));
  // A-F
  is(! isblank('\x41'));
  is(! isblank('\x46'));
  // G-Z
  is(! isblank('\x47'));
  is(! isblank('\x5A'));
  // symbols
  is(! isblank('\x5B'));
  is(! isblank('\x60'));
  // a-f
  is(! isblank('\x61'));
  is(! isblank('\x66'));
  // g-z
  is(! isblank('\x67'));
  is(! isblank('\x7A'));
  // symbols
  is(! isblank('\x7B'));
  is(! isblank('\x7E'));
  // rubout
  is(! isblank('\x7F'));
  }#)

(test test-ctype-isprint
  #2{
  // control
  is(! isprint('\x00'));
  is(! isprint('\x08'));
  // tab
  is(! isprint('\x09'));
  // whitespaces
  is(! isprint('\x0A'));
  is(! isprint('\x0D'));
  // control
  is(! isprint('\x0E'));
  is(! isprint('\x1F'));
  // space
  is(isprint('\x20'));
  // symbols
  is(isprint('\x21'));
  is(isprint('\x2F'));
  // 0-9
  is(isprint('\x30'));
  is(isprint('\x39'));
  // symbols
  is(isprint('\x3A'));
  is(isprint('\x40'));
  // A-F
  is(isprint('\x41'));
  is(isprint('\x46'));
  // G-Z
  is(isprint('\x47'));
  is(isprint('\x5A'));
  // symbols
  is(isprint('\x5B'));
  is(isprint('\x60'));
  // a-f
  is(isprint('\x61'));
  is(isprint('\x66'));
  // g-z
  is(isprint('\x67'));
  is(isprint('\x7A'));
  // symbols
  is(isprint('\x7B'));
  is(isprint('\x7E'));
  // rubout
  is(! isprint('\x7F'));
  }#)

(test test-ctype-ispunct
  #2{
  // control
  is(! ispunct('\x00'));
  is(! ispunct('\x08'));
  // tab
  is(! ispunct('\x09'));
  // whitespaces
  is(! ispunct('\x0A'));
  is(! ispunct('\x0D'));
  // control
  is(! ispunct('\x0E'));
  is(! ispunct('\x1F'));
  // space
  is(! ispunct('\x20'));
  // symbols
  is(ispunct('\x21'));
  is(ispunct('\x2F'));
  // 0-9
  is(! ispunct('\x30'));
  is(! ispunct('\x39'));
  // symbols
  is(ispunct('\x3A'));
  is(ispunct('\x40'));
  // A-F
  is(! ispunct('\x41'));
  is(! ispunct('\x46'));
  // G-Z
  is(! ispunct('\x47'));
  is(! ispunct('\x5A'));
  // symbols
  is(ispunct('\x5B'));
  is(ispunct('\x60'));
  // a-f
  is(! ispunct('\x61'));
  is(! ispunct('\x66'));
  // g-z
  is(! ispunct('\x67'));
  is(! ispunct('\x7A'));
  // symbols
  is(ispunct('\x7B'));
  is(ispunct('\x7E'));
  // rubout
  is(! ispunct('\x7F'));
  }#)

(test test-ctype-tolower
  #{
  is(char=(#\a, tolower(#\A)));
  is(char=(#\b, tolower(#\B)));
  is(char=(#\c, tolower(#\C)));
  is(char=(#\z, tolower(#\Z)));
  is(char=(#\a, tolower(#\a)));
  is(char=(#\b, tolower(#\b)));
  is(char=(#\c, tolower(#\c)));
  is(char=(#\z, tolower(#\z)));
  is(char=(#\!, tolower(#\!)));
  is(char=(#\?, tolower(#\?)));
  is(char=(#\ , tolower(#\ )));
  is(char=(#\tab, tolower(#\tab)));
  }#)

(test test-ctype-toupper
  #{
  is(char=(#\A, toupper(#\A)));
  is(char=(#\B, toupper(#\B)));
  is(char=(#\C, toupper(#\C)));
  is(char=(#\Z, toupper(#\Z)));
  is(char=(#\A, toupper(#\a)));
  is(char=(#\B, toupper(#\b)));
  is(char=(#\C, toupper(#\c)));
  is(char=(#\Z, toupper(#\z)));
  is(char=(#\!, toupper(#\!)));
  is(char=(#\?, toupper(#\?)));
  is(char=(#\ , toupper(#\ )));
  is(char=(#\tab, toupper(#\tab)));
  }#)
