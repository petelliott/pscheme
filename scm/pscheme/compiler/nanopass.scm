(define-library (pscheme compiler nanopass)
  (import (scheme base)
          (scheme cxr)
          (pscheme compiler util)
          (pscheme compiler file)
          (pscheme compiler error)
          (srfi 1)
          (srfi 28))
  (export language
          edit-language
          language-lazy-parse
          language-parse
          parse->string
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

    ;; parse-tree types

    (define-record-type nonterminal-node
      (make-nt-node rule alt args)
      nonterminal-node?
      (rule nt-node-rule)
      (alt nt-node-alt)
      (args nt-node-args))

    (define-record-type terminal-node
      (make-t-node rule pred val)
      terminal-node?
      (rule t-node-rule)
      (pred t-node-pred)
      (val t-node-val))

    (define (parse-node? obj)
      (or (nonterminal-node? obj)
          (terminal-node? obj)))

    (define (node-rule obj)
      (cond
       ((terminal-node? obj) (t-node-rule obj))
       ((nonterminal-node? obj) (nt-node-rule obj))
       (else #f)))

    (define-record-type lazy-node
      (make-lazy-node raw cont)
      lazy-node?
      (raw lazy-node-raw)
      (cont lazy-node-cont))

    (define-syntax lazy
      (syntax-rules ()
        ((_ f e) (make-lazy-node f (lambda () e)))))

    (define (cont-parse lazy)
      ((lazy-node-cont lazy)))

    (define (force-parsed parsed)
      (cond
       ((lazy-node? parsed) (force-parsed (cont-parse parsed)))
       ((span? parsed) (force-parsed (span-form parsed)))
       ((terminal-node? parsed) parsed)
       ((nonterminal-node? parsed)
        (make-nt-node
         (nt-node-rule parsed)
         (nt-node-alt parsed)
         (recursive-sloppy-map force-parsed (nt-node-args parsed))))))

    ;; parsing and unparsing

    (define (parse-terminal term form)
      (if ((cadr term) form)
          (make-t-node (car term) (cadr term) form)
          (pscm-err "parsing terminal: expected ~a, got ~a" (car term) (unspan1 form))))

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
      (define uform (unspan1 forml))
      (cond
       ((null? uform) '())
       ((not (pair? uform))
        (proc forml))
       ((proc (car uform)) =>
        (lambda (parsed)
          (cons parsed
                (result-map proc (cdr uform)))))
       (else #f)))

    (define (is-splicing? clause)
      (and (pair? clause)
           (pair? (car clause))
           (eq? (caar clause) 'unquote-splicing)))

    (define-syntax preserve-span
      (syntax-rules ()
        ((_ (name maybe-span) body ...)
         (let ((ms maybe-span))
           (if (span? ms)
               (copy-span ms
                          (let ((name (unspan1 ms)))
                            (parameterize ((current-span  ms))
                              body ...)))
               (let ((name ms))
                 body ...))))))


    (define (parse-clause l nt clause form)
      (define (inner clause form)
        (define uform (unspan1 form))
        (cond
         ((is-splicing? clause)
          (list (result-map (lambda (f)
                              (language-lazy-parse l f (cadar clause)))
                            form)))
         ((is-syntax? 'unquote clause)
          (let ((alt (assoc (cadr clause) l)))
            (if (and (terminal? alt) (not ((cadr alt) (strip-spans form))))
                #f
                (list (language-lazy-parse l form (cadr clause))))))
         ((and (pair? clause) (pair? uform))
          (merge-results (inner (car clause) (car uform))
                         (inner (cdr clause) (cdr uform))))
         (else
          (if (equal? clause (strip-spans form)) '() #f))))
      (define result (inner clause form))
      (if result
          (make-nt-node nt clause result)
          #f))

    (define (parse-nonterminal l nt form)
      (define (inner clauses form)
        (cond
         ((null? clauses) (pscm-err "parsing nonterminal: expected ~a, got ~a" (car nt) (strip-spans form)))
         ((parse-clause l (car nt) (car clauses) form))
         (else (inner (cdr clauses) form))))
      (inner (cdr nt) form))

    (define (default-alternative l)
      (if (terminal? (car l))
          (default-alternative (cdr l))
          (caar l)))

    (define (language-lazy-parse l form . rest)
      (define alt-name (or (and (pair? rest) (car rest))
                           (default-alternative l)))
      (define alt (assoc alt-name l))
      (unless alt
        (error "can't find alternative" alt-name))
      (if (terminal? alt)
          (lazy form (preserve-span (rform form) (parse-terminal alt rform)))
          (lazy form (preserve-span (rform form) (parse-nonterminal l alt rform)))))

    (define (language-parse l form . rest)
      (force-parsed (apply language-lazy-parse l form rest)))

    (define (language-unparse1 parsed proc)
      (define forms (and (nonterminal-node? parsed) (nt-node-args parsed)))
      (define (nonterminal pat)
        (define child (and (pair? forms) (car forms)))
        (cond
         ((is-splicing? pat)
          (set! forms (cdr forms))
          (sloppy-map proc child))
         ((is-syntax? 'unquote pat)
          (set! forms (cdr forms))
          (proc child))
         ((pair? pat)
          (cons (nonterminal (car pat))
                (nonterminal (cdr pat))))
         (else pat)))
      (if (terminal-node? parsed)
          (t-node-val parsed)
          (nonterminal (nt-node-alt parsed))))

    (define (language-unparse parsed)
      (language-unparse1 (force-parsed parsed) language-unparse))

    (define (parse->string parsed)
      (cond
       ((parse-node? parsed)
        (format "@~a{~a}"
                (node-rule parsed)
                (language-unparse1 parsed parse->string)))
       ((span? parsed)
        (parse->string (span-form parsed)))
       (else (format "~a" parsed))))

    ;; passes

    (define (flatten-list-args args)
      (sloppy-map
       (lambda (args)
         (if (or (pair? args) (null? args))
             (lambda rest
               (cond
                ((null? rest)
                 (sloppy-map (lambda (f) (f)) args))
                ((eq? (car rest) 'raw)
                 (sloppy-map (lambda (f) (f 'raw)) args))
                ((eq? (car rest) 'span)
                 (sloppy-map (lambda (f) (f 'span)) args))
                (else (error "unexpected arguments to pass recursion:" rest))))
             args))
       args))

    (define-syntax pass-cases
      (syntax-rules ()
        ((_ rec parsed ())
         (language-unparse1 parsed (lambda (form) ((rec form)))))
        ((_ rec parsed ((form vars body ...) rest ...))
         (if (equal? 'form (nt-node-alt parsed))
             (apply (lambda vars body ...) (flatten-list-args (recursive-sloppy-map rec (nt-node-args parsed))))
             (pass-cases rec parsed (rest ...))))))

    (define-syntax pass-rules
      (syntax-rules ($)
        ((_ rec parsed ())
         (language-unparse1 parsed (lambda (form) ((rec form)))))
        ((_ rec parsed (($ term vars body ...) rest ...))
         (if (equal? 'term (node-rule parsed))
             ((lambda vars body ...) (t-node-val parsed))
             (pass-rules rec parsed (rest ...))))
        ((_ rec parsed ((nonterm cases ...) rest ...))
         (if (equal? 'nonterm (node-rule parsed))
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
                               (let ((parsed (cont-parse lazy-form)))
                                 (preserve-span (parsed2 parsed)
                                   (pass-rules rec parsed2 (rules ...)))))
                              ((eq? (car rest) 'raw)
                               (strip-spans (lazy-node-raw lazy-form)))
                              ((eq? (car rest) 'span)
                               (lazy-node-raw lazy-form))
                              (else (error "unexpected arguments to pass recursion:" rest)))))))
             ((rec parsed)))))))

    (define-syntax define-pass
      (syntax-rules ()
        ((_ name (l0) rules ...)
         (define name (pass (l0) rules ...)))))

    (define (concat-passes . passes)
      (lambda (form)
        (define (inner p f)
          (if (null? p)
              f
              (inner (cdr p) ((car p) f))))
        (inner passes form)))

    ))
