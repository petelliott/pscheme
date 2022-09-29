(define-library (pscheme options)
  (import (scheme base))
  (export options)
  (begin

    (define-syntax options
      (syntax-rules ()
        ((_ list (name default) (n2 d2) ...)
         (begin
           (define name (if (null? list) default (car list)))
           (options (if (null? list) list (cdr list)) (n2 d2) ...)))
        ((_ list)
         (begin))))

    ))
