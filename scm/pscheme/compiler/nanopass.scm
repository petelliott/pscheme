(define-library (pscheme compiler nanopass)
  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (pscheme compiler util)
          (srfi 1))
  (export language
          edit-language
          language-lazy-parse
          language-parse
          language-unparse
          pass
          define-pass
          concat-passes)
  (begin

    ;; lanuage manipulation functions

    (define (terminal? clause)
      (and (pair? clause)
           (procedure? (cadr clause))
           (null? (cddr clause))))

    (define (language-add1 l plus)
      (cond
       ((terminal? plus) (cons plus l))
       ((null? l) (list plus))
       ((eq? (caar l) (car plus))
        (cons (cons (caar l) (append (cdr plus) (cdar l)))
              (cdr l)))
       (else
        (cons (car l)
              (language-add1 (cdr l) plus)))))

    (define (language-add l plus)
      (if (null? plus)
          l
          (language-add (language-add1 l (car plus)) (cdr plus))))

    (define (terminal-equal? a b)
      (and (terminal? a)
           (terminal? b)
           (eq? (car a) (car b))))

    (define (language-del1 l minus)
      (cond
       ((null? l) l)
       ((terminal-equal? (car l) minus)
        (cdr l))
       ((eq? (caar l) (car minus))
        (let ((clauses
               (filter (lambda (clause)
                         (not (member clause (cdr minus))))
                       (cdar l))))
          (if (null? clauses)
              (cdr l)
              (cons (cons (caar l) clauses)
                    (cdr l)))))
       (else
        (cons (car l)
              (language-del1 (cdr l) minus)))))

    (define (language-del l minus)
      (if (null? minus)
          l
          (language-del (language-del1 l (car minus)) (cdr minus))))

    ;; language helper syntax

    (define-syntax language
      (syntax-rules ()
        ((_) '())
        ((_ (name (clause ...) ...) rest ...)
         (cons '(name (clause ...) ...)
               (language rest ...)))
        ((_ (name proc) rest ...)
         (cons `(name ,proc)
               (language rest ...)))))

    (define-syntax edit-language
      (syntax-rules (+ -)
        ((_ l0) l0)
        ((_ l0 (+ transform ...) rest ...)
         (edit-language (language-add l0 (language transform ...)) rest ...))
        ((_ l0 (- transform ...) rest ...)
         (edit-language (language-del l0 (language transform ...)) rest ...))))

    ;; lazy parsing primitives

    (define-syntax lazy
      (syntax-rules ()
        ((_ f e) (cons f (lambda () e)))))

    (define (cont-parse lazy)
      ((cdr lazy)))

    (define (dont-parse lazy)
      (car lazy))

    (define (is-lazy? form)
      (and (pair? form)
           (procedure? (cdr form))))

    (define (force-parsed parsed)
      (cond
       ((is-lazy? parsed) (force-parsed (cont-parse parsed)))
       ((procedure? (cadr parsed)) parsed)
       (else
        (mcons (car parsed)
               (cadr parsed)
               (nested-map-lazy force-parsed (cddr parsed))))))

    ;; parsing and unparsing

    (define (parse-terminal term form)
      (if ((cadr term) form)
          `(,@term ,form)
          (error "got someting other than" term form)))

    (define-syntax merge-results
      (syntax-rules ()
        ((_ a b)
         (let ((left a))
           (if left
               (let ((right b))
                 (if right
                     (append left right)
                     #f))
               #f)))))

    (define (result->list r)
      (and r (list r)))

    (define (result-map proc forml)
      (cond
       ((null? forml) '())
       ((not (pair? forml))
        (proc forml))
       ((proc (car forml)) =>
        (lambda (parsed)
          (cons parsed
                (result-map proc (cdr forml)))))
       (else #f)))

    (define (is-splicing? clause)
      (and (pair? clause)
           (pair? (car clause))
           (eq? (caar clause) 'unquote-splicing)))

    (define (parse-clause l nt clause form)
      (define (inner clause form)
        (cond
         ((is-splicing? clause)
          (list (result-map (lambda (f)
                              (language-lazy-parse l f (cadar clause)))
                            form)))
         ((is-syntax? 'unquote clause)
          (let ((alt (assoc (cadr clause) l)))
            (if (and (terminal? alt) (not ((cadr alt) form)))
                #f
                (list (language-lazy-parse l form (cadr clause))))))
         ((and (pair? clause) (pair? form))
          (merge-results (inner (car clause) (car form))
                         (inner (cdr clause) (cdr form))))
         (else
          (if (equal? clause form) '() #f))))
      (define result (inner clause form))
      (if result
          `(,nt ,clause ,@result)
          #f))

    (define (parse-nonterminal l nt form)
      (define (inner clauses form)
        (cond
         ((null? clauses) (error "couldn't match nt" nt form))
         ((parse-clause l (car nt) (car clauses) form))
         (else (inner (cdr clauses) form))))
      (inner (cdr nt) form))

    (define (default-alternative l)
      (if (terminal? (car l))
          (default-alternative (cdr l))
          (caar l)))

    (define (language-lazy-parse l form . rest)
      (define a (write rest))
      (define b (newline))
      (define alt-name (or (and (pair? rest) (car rest))
                           (default-alternative l)))
      (define alt (assoc alt-name l))
      (unless alt
        (error "can't find alternative" alt-name))
      (if (terminal? alt)
          (lazy form (parse-terminal alt form))
          (lazy form (parse-nonterminal l alt form))))

    (define (language-parse l form . rest)
      (force-parsed (apply language-lazy-parse l form rest)))

    ;; helper functions for traversing argument lists

    (define (is-nested? lst)
      (and (pair? lst)
           (pair? (car lst))))

    (define (spliced-map proc lst)
      (cond
       ((null? lst) '())
       ((symbol? (car lst))
        (proc lst))
       (else
        (cons (proc (car lst))
              (spliced-map proc (cdr lst))))))

    (define (nested-map proc l)
      (cond
       ((null? l) '())
       ((is-nested? (car l))
        (cons (spliced-map proc (car l))
              (nested-map proc (cdr l))))
       (else
        (cons (proc (car l))
              (nested-map proc (cdr l))))))

    (define (is-nested-lazy? lst)
      (and (pair? lst)
           (is-lazy? (car lst))))

    (define (spliced-map-lazy proc lst)
      (cond
       ((null? lst) '())
       ((is-lazy? lst)
        (proc lst))
       (else
        (cons (proc (car lst))
              (spliced-map proc (cdr lst))))))

    (define (nested-map-lazy proc l)
      (cond
       ((null? l) '())
       ((is-nested-lazy? (car l))
        (cons (spliced-map-lazy proc (car l))
              (nested-map-lazy proc (cdr l))))
       (else
        (cons (proc (car l))
              (nested-map-lazy proc (cdr l))))))

    (define (language-unparse1 parsed proc)
      (define forms (cddr parsed))
      (define (nonterminal pat)
        (define child (and (pair? forms) (car forms)))
        (cond
         ((is-splicing? pat)
          (set! forms (cdr forms))
          (spliced-map proc child))
         ((is-syntax? 'unquote pat)
          (set! forms (cdr forms))
          (proc child))
         ((pair? pat)
          (cons (nonterminal (car pat))
                (nonterminal (cdr pat))))
         (else pat)))
      (if (procedure? (cadr parsed))
          (lambda () (caddr parsed))
          (nonterminal (cadr parsed))))

    (define (language-unparse parsed)
      (language-unparse1 (force-parsed parsed) language-unparse))

    ;; passes

    (define (flatten-list-args args)
      (sloppy-map
       (lambda (args)
         (if (pair? args)
             (let ((raw (sloppy-map (lambda (f) (f 'raw)) args)))
               (lambda rest
                 (cond
                  ((null? rest)
                   (sloppy-map (lambda (f) (f)) args))
                  ((eq? (car rest) 'raw)
                   raw)
                  (else (error "unexpected arguments to pass recursion:" rest)))))
             args))
       args))

    (define-syntax pass-cases
      (syntax-rules ()
        ((_ rec parsed ())
         ((language-unparse1 parsed rec)))
        ((_ rec parsed ((form vars body ...) rest ...))
         (if (equal? 'form (cadr parsed))
             (apply (lambda vars body ...) (flatten-list-args (nested-map-lazy rec (cddr parsed))))
             (pass-cases rec parsed (rest ...))))))

    (define-syntax pass-rules
      (syntax-rules ($)
        ((_ rec parsed ())
         ((language-unparse1 parsed rec)))
        ((_ rec parsed (($ term vars body ...) rest ...))
         (if (equal? 'term (car parsed))
             ((lambda vars body ...) (caddr parsed))
             (pass-rules rec parsed (rest ...))))
        ((_ rec parsed ((nonterm cases ...) rest ...))
         (if (equal? 'nonterm (car parsed))
             (pass-cases rec parsed (cases ...))
             (pass-rules rec parsed (rest ...))))))

    (define-syntax pass
      (syntax-rules ()
        ((_ (l0) rules ...)
         (lambda (form)
           (letrec ((parsed (language-lazy-parse l0 form))
                    (rec (lambda (lazy-form)
                           (lambda rest
                             (cond
                              ((null? rest)
                               (pass-rules rec (cont-parse lazy-form) (rules ...)))
                              ((eq? (car rest) 'raw)
                               (dont-parse lazy-form))
                              (else (error "unexpected arguments to pass recursion:" rest)))))))
             ((rec parsed)))))))

    (define-syntax define-pass
      (syntax-rules ()
        ((_ name (l0) rules ...)
         (define name (pass (l0) rules ...)))))

    (define (concat-passes passes)
      (lambda (form)
        (define (inner p f)
          (if (null? p)
              f
              (inner (cdr p) ((car p) f))))
        (inner passes form)))

    ))
