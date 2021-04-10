(define-library (pscheme compiler arch)
  (import (scheme base)
          (scheme file)
          (gauche base)) ;; TODO remove
  (export compile-environment
          enter-block-environment
          emit
          emit-eval
          unique
          genlabel)
  (begin

    (define (make-unique)
      (define n 0)
      (lambda ()
        (set! n (+ n 1))
        n))

    (define port-param (make-parameter #f))
    (define real-port-param (make-parameter #f))
    (define arch-param (make-parameter '()))
    (define unique (make-parameter (make-unique)))

    (define (genlabel prefix)
      (format "~a_~a" prefix ((unique))))

    (define (maybe-call-with-output-file filename proc)
      (if (eq? filename 'stdout)
          (proc (current-output-port))
          (call-with-output-file filename proc)))

    (define (compile-environment filename arch proc)
      (maybe-call-with-output-file filename
        (lambda (port)
          (parameterize ((port-param port)
                         (real-port-param port)
                         (arch-param arch)
                         (unique (make-unique)))
            (proc)))))

    (define (enter-block-environment proc)
      (write-string (call-with-output-string
                      (lambda (port)
                        (parameterize ((port-param port))
                          (proc))))
                    (real-port-param)))

    (define (emit key . rest)
      (write-string (apply (cdr (assq key (arch-param))) rest)
                    (port-param)))

    (define (emit-eval key . rest)
      (apply (cdr (assq key (arch-param))) rest))


    ))

;; Conventions of emit
;;
;; 1. a symbol 'result indicates the register that holds the result of the last expression executed
;; 2. (stack n) indicates an offset from the base pointer (locals)
;; 2. (temp n) indicates an offset from the stack pointer (nested expressions)
;; 2. (arg n) indicates the nth argument.
;; 2. (closure n) indicates the nth element of the closure.
;; 4. (global 'symbol) indicates a global symbol reference.
;; 5. a number will be treated as an immediate
