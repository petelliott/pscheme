(define-library (scheme read)
  (import (scheme base)
          (pscheme compiler file)
          (pscheme options))
  (export read)
  (begin

    (define (read . rest)
      (options rest (port (current-input-port)))
      (normal-read port))

    ))
