(import (scheme base)
        (pscheme test))


(define-syntax fake-quote
  (syntax-rules ()
    ((_ form ...) (quote (form ...)))))

(define-test "fake quote"
  (assert (equal? (fake-quote (1 2) (3 4)) '((1 2) (3 4)))))

(define-syntax alts
  (syntax-rules (a b)
    ((_ a) 1)
    ((_ b) 2)
    ((_ c) 3)))

(define-test "alternates"
  (assert (equal? (alts a) 1))
  (assert (equal? (alts b) 2))
  (assert (equal? (alts 3) 3)))

(define-syntax nest
  (syntax-rules ()
    ((nest a b) a)))

(define-test "nested"
  (assert (equal? (fake-quote (alts a)) '((alts a))))
  (assert (equal? (nest (nest 1 2) (nest 3 4)) 1)))

(define-syntax hyg
  (syntax-rules ()
    ((_ name)
     (let* ((name 5)
            (a 7))
       name))))

(define-test "hygene"
  (assert (equal? (hyg a) 5)))

(define-syntax global-hyg
  (syntax-rules ()
    ((_ name val)
     (begin
       (define a val)
       (define (name) a)))))

(global-hyg a 1)
(global-hyg b 2)
(global-hyg c 3)

(define-test "global hygene"
  (assert (equal? (a) 1))
  (assert (equal? (b) 2))
  (assert (equal? (c) 3)))

(define-syntax nested-ellipsis
  (syntax-rules ()
    ((_ (a b ...) ...)
     '((a b ...) ...))))

(define-test "nested ellipsis"
  (assert (equal? (nested-ellipsis (1 2 3 4) (5 6 7 8) (9 10 11 12))
                  '((1 2 3 4) (5 6 7 8) (9 10 11 12)))))

(define-syntax nested-ellipsis2
  (syntax-rules ()
    ((_ ((b ...) ...) ...)
     '(((b ...) ...) ...))))

(define-test "unprefixed nested ellipsis"
  (assert (equal? (nested-ellipsis2 ((1 2) (3 4)) ((5 6) (7 8)) ((9 10) (11 12)))
                  '(((1 2) (3 4)) ((5 6) (7 8)) ((9 10) (11 12))))))

(finish-tests)
