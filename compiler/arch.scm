(define-library (pscheme arch)
  (import (scheme base)
          (scheme file)
          (gauche base)) ;; TODO remove
  (export compile-environment
          emit
          unique)
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

    (define (compile-environment filename arch proc)
      (call-with-output-file filename
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



    ))

;; Conventions of emit
;;
;; 1. a symbol 'result indicates the register that holds the result of the last expression executed
;; 2. (b n) indicates an offset from the base pointer (locals)
;; 2. (a n) indicates the nth argument.
;; 3. (s n) indicates an offset from the stack pointer (n is positive no matter the direction of the stack)
;; 4. (global 'symbol) indicates a global symbol reference
;; 5. a number will be treated as an immediate
