#if `(MEMBER (READTABLE-CASE *READTABLE*) '(:DOWNCASE :PRESERVE))
#define str(s) #s
#define xstr(s) str(s)
#define debug(s, t) printf("x" # s "= %d, x" # t "= %s", \
                              x ## s, x ## t)
#define INCFILE(n)  vers ## n
#define glue(a, b)  a ## b
#define xglue(a, b) glue(a, b)
#define HIGHLOW     "hello"
#define LOW         LOW ", world"
#elif `(MEMBER (READTABLE-CASE *READTABLE*) '(:UPCASE :INVERT))
#define STR(s) #s
#define XSTR(s) STR(s)
#define DEBUG(s, t) PRINTF("x" # s "= %d, x" # t "= %s", \
                              X ## s, X ## t)
#define INCFILE(n)  VERS ## n
#define GLUE(a, b)  a ## b
#define XGLUE(a, b) GLUE(a, b)
#define HIGHLOW     "hello"
#define LOW         LOW ", world"
#endif
