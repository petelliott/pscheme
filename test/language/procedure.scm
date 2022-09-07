(import (scheme base)
        (pscheme test))

(define-test "identity procedure"
  (define (f x)
    x)

  (assert (equal? (f 1) 1))
  (assert (equal? (f f) f)))

(define-test "rest args"
  (define (f a b . rest)
    rest)

  (assert (equal? (f 1 2) '()))
  (assert (equal? (f 1 2 3 4 5) '(3 4 5))))

(define-test "thunk"
  (define (f proc a)
    (proc a))

  (assert (equal? (f (lambda (a) a) 6) 6))
  (assert (equal? (f (lambda (a) (+ 1 a)) 6) 7)))

(define-test "immediate lambda"
  (assert (equal? ((lambda (a b) (+ a b)) 5 6) 11)))

(finish-tests)
