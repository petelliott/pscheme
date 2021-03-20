(define-library (pscheme util)
  (import (scheme base))
  (export is-syntax?)
  (begin

    (define (is-syntax? sym form)
      (and (pair? form)
           (eq? (car form) sym)))

    ))
