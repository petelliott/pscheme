(define-library (scheme base)
  (export * + - <=
          cons car cdr pair? null?
          newline
          write
          eq?)
  (begin

    (define (eq? a b)
      (builtin eq? a b))

    ;;; 6.4: Pairs and lists

    ;; written in C: cons, car, cdr, pair?

    (define (null? obj)
      (eq? obj '()))

    ))
