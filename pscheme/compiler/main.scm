
(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme read)
        (scheme write))

(import (pscheme compiler arch)
        (pscheme compiler arch x86_64)
        (pscheme compiler frontend)
        (pscheme compiler codegen))

(define (read-all)
  (define obj (read))
  (if (eof-object? obj)
      '()
      (cons obj (read-all))))

(define program (read-all))

(define frontended (map frontend program))

#;(for-each (lambda (stmt) (write stmt) (newline))
          frontended)

(compile-environment 'stdout x86_64
                     (lambda ()
                       (codegen-main-file frontended)))
