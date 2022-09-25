(import (scheme base)
        (pscheme test))

(define-record-type rec
  (make-rec a b)
  rec?
  (a rec-a rec-set-a!)
  (b rec-b))

(define-record-type r2
  (make-r2)
  r2?)

(define r1 (make-rec 5 '()))
(define r2 (make-r2))

(define-test "record predicates"
  (assert (rec? r1))
  (assert (r2? r2))
  (assert (not (rec? r2)))
  (assert (not (r2? r1)))
  (assert (not (rec? '(rec)))))

(define-test "record ref"
  (assert (eq? (rec-a r1) 5))
  (assert (eq? (rec-b r1) '())))

(define-test "record set"
  (rec-set-a! r1 48)
  (assert (eq? (rec-a r1) 48))
  (assert (eq? (rec-b r1) '())))
