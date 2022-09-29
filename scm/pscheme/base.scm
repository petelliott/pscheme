(define-library (pscheme base)
  (import (scheme base))
  (export assoc-ref)
  (begin

    (define (assoc-ref key alist)
      (define result (assoc key alist))
      (if result (cdr result) result))

    ))
