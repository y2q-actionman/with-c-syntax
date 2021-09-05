#define debug(...)	fprintf(stderr, __VA_ARGS__)
#define showlist(...)	puts(#__VA_ARGS__)
#define report(test, ...)	((test)?puts(#test):\
				 printf(__VA_ARGS__))
