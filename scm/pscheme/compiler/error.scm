(define-library (pscheme compiler error)
  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (pscheme compiler file)
          (srfi 28))
  (export pscheme-error-span
          pscm-err
          pscm-warn)
  (begin

    (define-record-type pscheme-error
      (make-pscheme-error warning span fmt args)
      pscheme-error?
      (warning error-warning)
      (span error-span)
      (fmt error-fmt)
      (args error-args))

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
        ;; TODO: print the span
        (newline)))

    (define pscheme-error-span (make-parameter #f))

    (define (pscm-err . args)
      (print-error
       (if (span? (car args))
           (make-pscheme-error #f (car args) (cadr args) (cddr args))
           (make-pscheme-error #f (pscheme-error-span) (car args) (cdr args))))
      (exit #f))

    (define (pscm-warn . args)
      (print-error
       (if (span? (car args))
           (make-pscheme-error #t (car args) (cadr args) (cddr args))
           (make-pscheme-error #t (pscheme-error-span) (car args) (cdr args)))))

    ))
