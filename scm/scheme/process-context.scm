(define-library (scheme process-context)
  (import (scheme base)
          (pscheme ffi))
  (export exit)
  (begin

    (define cexit (ff->scheme void exit (int status)))

    (define (exit . rest)
      (cond
       ((null? rest) (cexit 0))
       ((eq? (car rest) #t) (cexit 0))
       ((eq? (car rest) #f) (cexit 1))
       (else (cexit (car rest)))))

    ))
