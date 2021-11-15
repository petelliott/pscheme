(define-library (scheme base)
  (export * + - <=
          cons car cdr caar cadr cdar cddr pair? null?
          newline
          write
          eq?
          cond case and or when unless let let* letrec letrec*)
  (begin

    ;;; 6.1: Equivalence predicates

    (define (eq? a b)
      (builtin eq? a b))

    ;;; 6.4: Pairs and lists

    ;; written in C: cons, car, cdr, pair?

    (define (caar p)
      (car (car p)))

    (define (cadr p)
      (car (cdr p)))

    (define (cdar p)
      (cdr (car p)))

    (define (cddr p)
      (cdr (cdr p)))

    (define (null? obj)
      (eq? obj '()))

    ;;; 7.3: Derived expression types

    ;; all of these are copied verbaitm from the r7rs spec

    (define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))

    (define-syntax case
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key ’(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) => result))
         (if (memv key ’(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key ’(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key ’(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

    (define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...))))))

    (define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))

    (define-syntax let
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
         ((letrec ((tag (lambda (name ...)
                          body1 body2 ...)))
            tag)
          val ...))))

    (define-syntax let*
      (syntax-rules ()
        ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
           body1 body2 ...)
         (let ((name1 val1))
           (let* ((name2 val2) ...)
             body1 body2 ...)))))

    (define-syntax letrec
      (syntax-rules ()
        ((letrec ((var1 init1) ...) body ...)
         (letrec "generate temp names"
           (var1 ...)
           ()
           ((var1 init1) ...)
           body ...))
        ((letrec "generate temp names"
           ()
           (temp1 ...)
           ((var1 init1) ...)
           body ...)
         (let ((var1 #f) ...)
           (let ((temp1 init1) ...)
             (set! var1 temp1)
             ...
             body ...)))
        ((letrec "generate temp names"
           (x y ...)
           (temp ...)
           ((var1 init1) ...)
           body ...)
         (letrec "generate temp names"
           (y ...)
           (newtemp temp ...)
           ((var1 init1) ...)
           body ...))))

    (define-syntax letrec*
      (syntax-rules ()
        ((letrec* ((var1 init1) ...) body1 body2 ...)
         (let ((var1 #f) ...)
           (set! var1 init1)
           ...
           (let () body1 body2 ...)))))


    ))
