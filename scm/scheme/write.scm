(define-library (scheme write)
  (import (scheme base))
  (export write
          display)
  (begin

    (define (integer-write n)
      (cond
       ((negative? n)
        (write-char #\-)
        (integer-write (- n)))
       ((> n 0)
        (integer-write (quotient n 10))
        (write-char (integer->char (+ (remainder n 10) (char->integer #\0)))))))

    (define (list-write form display)
      (unified-write (car form) display)
      (cond
       ((pair? (cdr form))
        (write-char #\space)
        (list-write (cdr form) display))
       ((null? (cdr form)))
       (else
        (write-string " . ")
        (unified-write (cdr form) display))))

    ;; TODO: sanitize strings

    (define (string-write form display)
      (unless display
        (write-char #\"))
      (write-string form)
      (unless display
        (write-char #\")))

    (define (char-write form display)
      (if display
          (write-char form)
          (begin
            (write-string "#\\")
            (case form
              ((#\space) (write-string "space"))
              ((#\newline) (write-string "newline"))
              (else (write-char form))))))

    ;; TODO: use parameters
    (define (unified-write form display)
      (cond
       ((zero? form) (write-char #\0))
       ((integer? form) (integer-write form))
       ((pair? form)
        (write-char #\()
        (list-write form display)
        (write-char #\)))
       ((char? form) (char-write form display))
       ((string? form) (string-write form display))
       ((null? form) (write-string "()"))
       ((eq? form '#f) (write-string "#f"))
       ((eq? form '#t) (write-string "#t"))
       ((eq? form (begin)) (write-string "#<unspecified>"))
       (else (write-string "#<?>"))))

    (define (write form)
      (unified-write form #f))

    (define (display form)
      (unified-write form #t))

    ))
