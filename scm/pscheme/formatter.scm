(define-library (pscheme formatter)
  (import (scheme base)
          (scheme write)
          (pscheme base))
  (export sexp-format)
  (begin

    (define current-indent (make-parameter 0))

    (define (line)
      (display #\newline)
      (do ((i 0 (+ i 1)))
          ((> i (current-indent)))
          (display #\space)))

    (define (indent n)
      (current-indent (+ n (current-indent))))

    (define (list-format rules sexp forms)
      (inner-format rules (car sexp))
      (parameterize ((current-indent
                      (if (equal? forms 0)
                          (+ 2 (current-indent))
                          (current-indent))))
        (cond
         ((null? (cdr sexp)))
         ((pair? (cdr sexp))
          (if (and forms (<= forms 0))
              (line)
              (display #\space))
          (list-format rules (cdr sexp) (and forms (- forms 1))))
         (else
          (display " . ")
          (write (cdr sexp))))))

    (define (inner-format rules sexp)
      (if (pair? sexp)
          (begin
            (display "(")
            (list-format rules sexp (assoc-ref (car sexp) rules))
            (display ")"))
          (write sexp)))

    (define (sexp-format rules sexp)
      (parameterize ((current-indent 0))
        (inner-format rules sexp)))

    ))
