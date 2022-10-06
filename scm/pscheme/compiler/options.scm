(define-library (pscheme compiler options)
  (import (scheme base))
  (export opt-param
          option
          optionize)
  (begin

    (define opts '())

    (define (getdef rest)
      (if (null? rest)
          #f
          (car rest)))

    (define (opt-param tag . default)
      (define slot (assoc tag opts))
      (if slot
          (cdr slot)
          (begin
            (set! opts (cons (cons tag (make-parameter (getdef default))) opts))
            (cdar opts))))

    (define (option tag . default)
      ((apply opt-param tag default)))

    (define-syntax optionize
      (syntax-rules ()
        ((_ ((tag val) ...) body ...)
         (parameterize (((opt-param 'tag) val) ...)
           body ...))))

    ))
