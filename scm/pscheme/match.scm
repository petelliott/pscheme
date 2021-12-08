(define-library (pscheme match)
  (import (scheme base))
  (export match)
  (begin

    (define nomatch (cons 'unique 'value))

    (define-syntax match1
      (syntax-rules (unquote unquote-splicing)
        ((_ cont expr (unquote form))
         (let ((form expr))
           cont))
        ((_ cont expr (pattern-car (unquote-splicing pattern-cdr)))
         (match1 cont expr (pattern-car . (unquote pattern-cdr))))
        ((_ cont expr (pattern-car . pattern-cdr))
         (if (pair? expr)
             (let ((expr-car (car expr))
                   (expr-cdr (cdr expr)))
               (match1 (match1 cont expr-cdr pattern-cdr)
                       expr-car pattern-car))
             nomatch))
        ((_ cont expr pattern)
         (if (equal? 'pattern expr)
             cont
             nomatch))))

    (define-syntax match-chain
      (syntax-rules (else)
        ((_ expr (else case ...) rest ...)
         (begin case ...))
        ((_ expr (pattern case ...) rest ...)
         (let ((m (match1 (begin case ...) expr pattern)))
           (if (eq? m nomatch)
               (match-chain expr rest ...)
               m)))
        ((_ expr)
         (values))))

    (define-syntax match
      (syntax-rules ()
        ((_ expr case ...)
         (let ((e expr))
           (match-chain e case ...)))))

    ))
