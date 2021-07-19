#if `(MEMBER (READTABLE-CASE *READTABLE*) '(:DOWNCASE :PRESERVE))
#define debug(...)	fprintf(stderr, __VA_ARGS__)
#define showlist(...)	puts(#__VA_ARGS__)
#define report(test, ...)	((test)?puts(#test):\
				 printf(__VA_ARGS__))
#elif `(MEMBER (READTABLE-CASE *READTABLE*) '(:UPCASE :INVERT))
#define DEBUG(...)	FPRINTF(STDERR, __VA_ARGS__)
#define SHOWLIST(...)	PUTS(#__VA_ARGS__)
#define REPORT(test, ...)	((test)?PUTS(#test):\
				 PRINTF(__VA_ARGS__))
#endif
