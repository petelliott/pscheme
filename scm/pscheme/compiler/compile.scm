(define-library (pscheme compiler compile)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (srfi-28)
          (pscheme compiler arch)
          (pscheme compiler arch x86_64)
          (pscheme compiler frontend)
          (pscheme compiler codegen))
  (export compile-file)
  (begin

    (define (read-all . args)
      (define obj (apply read args))
      (if (eof-object? obj)
          '()
          (cons obj (apply read-all args))))

    (define (read-file filename)
      (call-with-input-file filename
        (lambda (port)
          (read-all port))))

    (define (compile-file target filename program-or-lib)
      (define ir (map frontend (read-file filename)))
      (write ir)
      (newline)
      (compile-environment (string-append filename ".s") target
                           (lambda ()
                             (case program-or-lib
                               ((program) (codegen-main-file ir))
                               ((library) (codegen-library-file ir))
                               (else (error "invalid argument to compile-file: " program-or-lib))))))

    ))
