#if `(MEMBER (READTABLE-CASE *READTABLE*) '(:DOWNCASE :PRESERVE))
#define offsetof `WITH-C-SYNTAX.SYNTAX:|__offsetof|
#elif `(MEMBER (READTABLE-CASE *READTABLE*) '(:UPCASE :INVERT))
#define OFFSETOF `WITH-C-SYNTAX.SYNTAX:|__offsetof|
#endif
