(define-library (scheme base)
  (export +
          <=
          cons
          car
          cdr
          newline
          write
          eq?)
  (begin

    (define (eq? a b)
      (builtin eq? a b))

    ))
