(define-library (pscheme compiler error)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)
          (pscheme compiler file)
          (srfi 28))
  (export pscm-err
          pscm-warn)
  (begin

    (define-record-type pscheme-error
      (make-pscheme-error warning span fmt args)
      pscheme-error?
      (warning error-warning)
      (span error-span)
      (fmt error-fmt)
      (args error-args))

    (define (print-line-number n)
      (display (make-string (- 5 (string-length (number->string n))) #\space))
      (display n)
      (display " "))

    (define (print-line text n span colour)
      (print-line-number n)
      (cond
       ((eof-object? text)
        (display colour)
        (display "EOF")
        (display "\x1b[0m"))
       ((and (= n (span-sr span))
             (= n (span-er span)))
        (display (substring text 0 (span-sc span)))
        (display colour)
        (display (substring text (span-sc span) (span-ec span)))
        (display "\x1b[0m")
        (display (substring text (span-ec span) (string-length text))))
       ((= n (span-sr span))
        (display (substring text 0 (span-sc span)))
        (display colour)
        (display (substring text (span-sc span) (string-length text)))
        (display "\x1b[0m"))
       ((= n (span-er span))
        (display colour)
        (display (substring text 0 (span-ec span)))
        (display "\x1b[0m")
        (display (substring text (span-ec span) (string-length text))))
       (else
        (display colour)
        (display text)
        (display "\x1b[0m")))
      (newline))

    (define (print-span span colour)
      (define (inner port n)
        (define line (read-line port))
        (cond
         ((and (>= n (span-sr span))
              (<= n (span-er span)))
          (print-line line n span colour)
          (inner port (+ n 1)))
         ((< n (span-sr span))
          (inner port (+ n 1)))))
      (call-with-input-file (span-file span)
        (lambda (port)
          (inner port 1))))

    (define (print-error error)
      (parameterize ((current-output-port (current-error-port)))
        (define colour (if (error-warning error) "\x1b[1;35m" "\x1b[1;31m"))
        (define span (error-span error))
        (when span
          (display (format "\x1b[1m~a:~a:~a: \x1b[0m"
                           (span-file span)
                           (span-sr span)
                           (span-sc span))))
        (display colour)
        (if (error-warning error)
            (display "warning: ")
            (display "error: "))
        (display "\x1b[0m")
        (display (apply format (error-fmt error)
                        (map (lambda (arg) (format "\x1b[1m~a\x1b[0m" arg))
                             (error-args error))))
        (newline)
        (when span
          (print-span span colour))))

    (define (pscm-err . args)
      (print-error
       (if (span? (car args))
           (make-pscheme-error #f (car args) (cadr args) (cddr args))
           (make-pscheme-error #f (current-span) (car args) (cdr args))))
      (exit #f))

    (define (pscm-warn . args)
      (print-error
       (if (span? (car args))
           (make-pscheme-error #t (car args) (cadr args) (cddr args))
           (make-pscheme-error #t (current-span) (car args) (cdr args)))))

    ))
