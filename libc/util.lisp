(in-package #:with-c-syntax.libc-implementation)

;;; At this point, the libc package was fixed. I build its cache here.
(build-libc-symbol-cache (find-package '#:with-c-syntax.libc))
