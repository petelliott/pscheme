(import (scheme base)
        (pscheme test))

(define-test "normal closure"
  (define (f)
    (define a 5)
    (lambda (b) (+ a b)))

  (assert (equal? ((f) 6) 11)))

(define-test "arg closure"
  (define (g a)
    (lambda (b) (+ a b)))

  (assert (equal? ((g 5) 6) 11)))

(define-test "self closure"
  (define (make-map proc)
    (define (m lst)
      (if (null? lst)
          '()
          (cons (proc (car lst))
                (m (cdr lst)))))
    m)

  (define add-list (make-map (lambda (i) (+ 1 i))))

  (assert (equal? (add-list '(1 2 3 4 5)) '(2 3 4 5 6))))

(define-test "mutable closures"
  (define (ctr)
    (define cnt 0)
    (lambda ()
        (set! cnt (+ cnt 1))
        cnt))

  (define c (ctr))
  (assert (equal? (c) 1))
  (assert (equal? (c) 2))
  (assert (equal? (c) 3))
  (assert (equal? (c) 4))
  (assert (equal? (c) 5)))

(define-test "mutable arg closures"
  (define (ctr2 cnt)
    (lambda ()
      (set! cnt (+ cnt 1))
      cnt))

  (define c2 (ctr2 4))
  (assert (equal? (c2) 5))
  (assert (equal? (c2) 6))
  (assert (equal? (c2) 7))
  (assert (equal? (c2) 8))
  (assert (equal? (c2) 9)))

(define-test "mutable rest closure" SKIP
  (define (ctr2 . cnt)
    (lambda ()
      (set! cnt (list (+ (car cnt) 1)))
      cnt))

  (define c2 (ctr2 4))
  (assert (equal? (c2) 5))
  (assert (equal? (c2) 6))
  (assert (equal? (c2) 7))
  (assert (equal? (c2) 8))
  (assert (equal? (c2) 9)))

(finish-tests)
