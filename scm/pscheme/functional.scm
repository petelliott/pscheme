(define-library (pscheme functional)
  (import (scheme base))
  (export find)
  (begin

    (define (find proc lst)
      (cond
       ((null? lst) #f)
       ((proc (car lst)) (car lst))
       (else (find proc (cdr lst)))))

    ))
