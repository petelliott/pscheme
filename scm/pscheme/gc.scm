(define-library (pscheme gc)
  (import (scheme base)
          (pscheme ffi))
  (export gc-collect
          gc-print-stats
          gc-auto-collect?
          gc-set-auto-collect!)
  (begin

    (define gc-collect (ff->scheme void pscheme_collect_garbage))
    (define gc-print-stats (ff->scheme void pscheme_print_gc_stats))
    (define gc-auto-collect? (ff->scheme bool pscheme_is_automatic_collection_enabled))
    (define gc-set-auto-collect! (ff->scheme void pscheme_set_automatic_collection (bool enabled)))

    ))
