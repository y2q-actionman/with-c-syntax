#ifndef STDARG_H
#define STDARG_H

#if `(MEMBER (READTABLE-CASE *READTABLE*) '(:DOWNCASE :PRESERVE))
#define va_arg(ap,type) (type)(`CL:POP(ap))
#elif `(MEMBER (READTABLE-CASE *READTABLE*) '(:UPCASE :INVERT))
#define VA_ARG(ap,type) (type)(`CL:POP(ap))
#endif

#endif
