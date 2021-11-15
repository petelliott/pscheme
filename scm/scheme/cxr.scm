(define-library (scheme cxr)
  (import (scheme base))
  (export caaaar caaar caaddr cadaar cadar cadddr cdaaar cdaar cdaddr cddaar
          cddar cddddr caaadr caadar caadr cadadr caddar caddr cdaadr cdadar
          cdadr cddadr cdddar cdddr)
  (begin

    (define (caaaar p)
      (car (caaar p)))

    (define (caaar p)
      (car (caar p)))

    (define (caaddr p)
      (car (caddr p)))

    (define (cadaar p)
      (car (cdaar p)))

    (define (cadar p)
      (car (cdar p)))

    (define (cadddr p)
      (car (cdddr p)))

    (define (cdaaar p)
      (cdr (caaar p)))

    (define (cdaar p)
      (cdr (caar p)))

    (define (cdaddr p)
      (cdr (caddr p)))

    (define (cddaar p)
      (cdr (cdaar p)))

    (define (cddar p)
      (cdr (cdar p)))

    (define (cddddr p)
      (cdr (cdddr p)))

    (define (caaadr p)
      (car (caadr p)))

    (define (caadar p)
      (car (cadar p)))

    (define (caadr p)
      (car (cadr p)))

    (define (cadadr p)
      (car (cdadr p)))

    (define (caddar p)
      (car (cddar p)))

    (define (caddr p)
      (car (cddr p)))

    (define (cdaadr p)
      (cdr (caadr p)))

    (define (cdadar p)
      (cdr (cadar p)))

    (define (cdadr p)
      (cdr (cadr p)))

    (define (cddadr p)
      (cdr (cdadr p)))

    (define (cdddar p)
      (cdr (cddar p)))

    (define (cdddr p)
      (cdr (cddr p)))

    ))
