(import (scheme base)
        (pscheme test))

(define global-var 1)

(define-test "variable reference"
  (define local-var 2)
  (assert (equal? global-var 1))
  (assert (equal? local-var 2)))

(define-test "literal"
  (assert (equal? 5 5))
  (assert (not (equal? 5 6)))
  (assert (equal? '(1 2 3 4) '(1 2 3 4)))
  (assert (not (equal? '(1 2 3 4) '(1 2 3 4 5))))
  (assert (equal? #\a #\a))
  (assert (not (equal? #\a #\b)))
  (assert (equal? "hey" "hey"))
  (assert (not (equal? "hey" "ho")))
  (assert (equal? #t #t))
  (assert (not (equal? #t #f))))

(define-test "symbol literal" SKIP
  (assert (eq? 'hey 'hey))
  (assert (not (eq? 'hey 'ho))))

(define-test "if"
  (assert (equal? (if #t 5 6) 5))
  (assert (equal? (if #t 5) 5))
  (assert (equal? (if #f 5 6) 6)))

(define global-set 1)
(define-test "assignment"
  (define a 1)
  (set! a 2)
  (set! global-set 2)
  (assert (equal? a 2))
  (assert (equal? global-set 2)))

(define-test "begin"
  (assert (equal? (begin 1 2 3 4) 4)))

(finish-tests)
