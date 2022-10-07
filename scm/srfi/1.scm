(define-library (srfi 1)
  (import (scheme base)
          (pscheme options))
  (export iota last zip fold filter find delete delete-duplicates)
  (begin

    ;;; Constructors

    (define (iota-inner count curr step)
      (if (eq? count 0)
          '()
          (cons curr (iota-inner (- count 1) (+ curr step) step))))

    (define (iota count . args)
      (options args (start 0) (step 1))
      (iota-inner count start step))

    ;;; Predicates
    ;;; Selectors

    (define (last list)
      (if (null? (cdr list))
          (car list)
          (last (cdr list))))

    ;;; Miscellaneous

    (define (any-null? lists)
      (if (null? lists)
          #f
          (or (null? (car lists))
              (any-null? (cdr lists)))))

    (define (cars lists)
      (if (null? lists)
          '()
          (cons (caar lists) (cars (cdr lists)))))

    (define (cdrs lists)
      (if (null? lists)
          '()
          (cons (cdar lists) (cars (cdr lists)))))

    (define (zip . lists)
      (if (any-null? lists)
          '()
          (cons (cars lists) (apply zip (cdrs lists)))))

    ;;; Fold, unfold & map

    (define (fold kons knil . lists)
      (if (any-null? lists)
          knil
          (apply fold kons (apply kons (append (cars lists) (list knil))) (cdrs lists))))


    ;;; Filtering & partitioning

    (define (filter pred list)
      (cond
       ((null? list) '())
       ((pred (car list))
        (cons (car list) (filter pred (cdr list))))
       (else (filter pred (cdr list)))))

    ;;; Searching

    (define (find pred lst)
      (cond
       ((null? lst) #f)
       ((pred (car lst)) (car lst))
       (else (find pred (cdr lst)))))

    ;;; Deleting

    (define (delete-inner x lst cmp)
      (cond
       ((null? lst) '())
       ((cmp x (car lst))
        (delete-inner x (cdr lst) cmp))
       (else
        (cons (car lst) (delete-inner x (cdr lst) cmp)))))

    (define (delete x lst . args)
      (options args (cmp equal?))
      (delete-inner x lst cmp))

    (define (delete-duplicates-inner lst cmp seen)
      (cond
       ((null? lst) '())
       ((member (car lst) seen cmp)
        (delete-duplicates-inner (cdr lst) cmp seen))
       (else
        (cons (car lst)
              (delete-duplicates-inner (cdr lst) cmp (cons (car lst) seen))))))

    (define (delete-duplicates lst . args)
      (options args (cmp equal?))
      (delete-duplicates-inner lst cmp '()))

    ;;; Set operations on lists
    ;;; primitive side-effects


    ))
