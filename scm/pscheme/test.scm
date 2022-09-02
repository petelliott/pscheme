(define-library (pscheme test)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (export assert
          skip-test
          run-test
          define-test
          finish-tests)
  (begin

    (define has-global-fail #f)
    (define has-local-fail #f)

    (define (assert b)
      (unless b
        (set! has-global-fail #t)
        (set! has-local-fail #t)))

    (define red    "31")
    (define green  "32")
    (define yellow "33")

    (define (bcd color obj)
      (display "\e[1;")
      (display color)
      (display "m")
      (display obj)
      (display "\e[0m"))

    (define (skip-test name)
      (bcd yellow "SKIP")
      (display " ")
      (display name)
      (newline))

    (define (run-test name body)
      (set! has-local-fail #f)
      (body)
      (if has-local-fail
          (bcd red "FAIL")
          (bcd green "PASS"))
      (display " ")
      (display name)
      (newline))

    (define-syntax define-test
      (syntax-rules (SKIP)
        ((_ name SKIP body ...)
         (skip-test name))
        ((_ name body ...)
         (run-test name (lambda () body ...)))))

    (define (finish-tests)
      (exit (not has-global-fail)))


    ))
