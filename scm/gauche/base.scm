(define-library (gauche base)
  (import (scheme base)
          (pscheme ffi))
  (export sys-system)
  (begin

    (define sys-system (ff->scheme int system (char* command)))

    ))
