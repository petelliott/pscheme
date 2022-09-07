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

(define-syntax hyg
  (syntax-rules ()
    ((_ name)
     (let* ((name 5)
            (a 7))
       name))))

(define-test "hygene" SKIP
  (assert (equal? (hyg a) 5)))

(finish-tests)
