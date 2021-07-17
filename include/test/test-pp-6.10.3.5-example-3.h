#if `(MEMBER (READTABLE-CASE *READTABLE*) '(:DOWNCASE :PRESERVE))
#define x	3
#define f(a)	f(x * (a))
#undef  x
#define x	2
#define g	f
#define z	z[0]
#define h	g(~
#define m(a)	a(w)
#define w	0,1
#define t(a)	a
#define p()	int
#define q(x)	x
#define r(x,y)	x ## y
#define str(x)	# x
#elif `(MEMBER (READTABLE-CASE *READTABLE*) '(:UPCASE :INVERT))
#define X	3
#define F(A)	F(X * (A))
#undef  X
#define X	2
#define G	F
#define Z	Z[0]
#define H	G(~
#define M(A)	A(W)
#define W	0,1
#define T(A)	A
#define P()	INT
#define Q(X)	X
#define R(X,Y)	X ## Y
#define STR(X)	# X
#endif
