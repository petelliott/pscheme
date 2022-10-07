(define-library (scheme process-context)
  (import (scheme base)
          (pscheme ffi))
  (export command-line exit)
  (begin

    (define command-line (ff->scheme pscheme_t pscheme_get_command_line))

    (define cexit (ff->scheme void exit (int status)))

    (define (exit . rest)
      (cond
       ((null? rest) (cexit 0))
       ((eq? (car rest) #t) (cexit 0))
       ((eq? (car rest) #f) (cexit 1))
       (else (cexit (car rest)))))

    ))
