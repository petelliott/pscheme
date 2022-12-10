(define-library (pscheme string)
  (import (scheme base)
          (scheme read))
  (export string-join
          string-starts-with
          string-split
          string->object)
  (begin

    (define (string-join sep strings)
      (if (null? strings)
          ""
          (apply string-append
                 (car strings)
                 (map (lambda (str) (string-append sep str))
                      (cdr strings)))))

    (define (string-starts-with str needle)
      (define slen (string-length str))
      (define nlen (string-length needle))
      (and (>= slen nlen)
           (string=? (substring str 0 nlen) needle)))

    (define (string-split str char)
      (define l (string-length str))
      (let loop ((i 0))
        (if (>= i l)
            '()
            (do ((j i (+ j 1)))
                ((or (= l j) (equal? (string-ref str j) char))
                 (cons (string-copy str i j) (loop (+ j 1))))))))

    (define (string->object str)
      (call-with-port (open-input-string str)
        (lambda (port)
          (read port))))

    ))
