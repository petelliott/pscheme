(define-library (scheme write)
  (import (scheme base))
  (export write)
  (begin

    (define (integer-write n)
      (cond
       ((negative? n)
        (write-char #\-)
        (integer-write (- n)))
       ((> n 0)
        (integer-write (quotient n 10))
        (write-char (integer->char (+ (remainder n 10) (char->integer #\0)))))))

    (define (list-write form)
      (write (car form))
      (cond
       ((pair? (cdr form))
        (write-char #\space)
        (list-write (cdr form)))
       ((null? (cdr form)))
       (else
        (write-string " . ")
        (write (cdr form)))))

    ;; TODO: sanitize strings

    (define (string-write form)
      (write-char #\")
      (write-string form)
      (write-char #\"))

    (define (char-write form)
      (write-string "#\\")
      (case form
        ((#\space) (write-string "space"))
        ((#\newline) (write-string "newline"))
        (else (write-char form))))

    (define (write form)
      (cond
       ((zero? form) (write-char #\0))
       ((integer? form) (integer-write form))
       ((pair? form)
        (write-char #\()
        (list-write form)
        (write-char #\)))
       ((char? form) (char-write form))
       ((string? form) (string-write form))
       ((null? form) (write-string "()"))
       ((eq? form '#f) (write-string "#f"))
       ((eq? form '#t) (write-string "#t"))
       ((eq? form (begin)) (write-string "#<unspecified>"))
       (else (write-string "#<?>"))))

    ))
