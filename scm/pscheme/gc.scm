(define-library (pscheme gc)
  (import (scheme base)
          (pscheme ffi))
  (export gc-collect
          gc-print-stats)
  (begin

    (define gc-collect (ff->scheme void pscheme_collect_garbage))
    (define gc-print-stats (ff->scheme void pscheme_print_gc_stats))

    ))
