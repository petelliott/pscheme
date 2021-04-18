(define-library (pscheme string)
  (import (scheme base))
  (export string-join)
  (begin

    (define (string-join sep strings)
      (apply string-append
             (cons (car strings)
                   (map (lambda (str) (string-append sep str))
                        (cdr strings)))))
    ))
