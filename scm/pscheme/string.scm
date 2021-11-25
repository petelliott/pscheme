(define-library (pscheme string)
  (import (scheme base))
  (export string-join
          string-starts-with)
  (begin

    (define (string-join sep strings)
      (apply string-append
             (cons (car strings)
                   (map (lambda (str) (string-append sep str))
                        (cdr strings)))))

    (define (string-starts-with str needle)
      (define slen (string-length str))
      (define nlen (string-length needle))
      (and (>= slen nlen)
           (string=? (substring str 0 nlen) needle)))
    ))
