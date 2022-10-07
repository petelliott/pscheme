(define-library (srfi 18)
  (import (scheme base))
  (export time? time->seconds seconds->time)
  (begin

    (define time? integer?)

    (define (time->seconds t)
      t)

    (define (seconds->time s)
      s)

    ))
