(define-library (pscheme gc)
  (import (scheme base)
          (pscheme ffi))
  (export gc-collect)
  (begin

    (define gc-collect (ff->scheme void pscheme_collect_garbage))

    ))
