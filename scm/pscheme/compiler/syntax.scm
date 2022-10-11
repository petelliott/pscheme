(define-library (pscheme compiler syntax)
  (import (scheme base)
          (scheme cxr)
          (pscheme compiler file)
          (pscheme compiler util)
          (pscheme compiler error)
          (srfi 1))
  (export make-syntax-rules
          syntax-node?
          syntax-node-sym
          syntax-node-env
          syntax-node-instance
          syntax-equal?
          transform-syntax)
  (begin

    (define (merge-matches . matches)
      (cond
       ((null? matches) '())
       ((not (car matches)) #f)
       (else
        (let ((merged (apply merge-matches (cdr matches))))
          (and merged
               (append (car matches) merged))))))

    (define (is-ellipsis? pattern)
      (and (pair? pattern)
           (pair? (cdr pattern))
           (eq? (cadr pattern) '...)))

    ;; addresses indicate a position in ellipses
    ;; so (1 2) would indicate the 3rd match of the 2nd match of a variable
    (define-record-type match
      (make-match name address form)
      match?
      (name match-name)
      (address match-address)
      (form match-form))

    ;;; matches syntax transformer patterns with ellipsis and whatnot
    (define (match-syntax-pattern pattern form literals)
      (define (inner pattern sform raddr)
        (define form (unspan1 sform))
        (cond
         ((and (null? pattern) (null? form)) '())
         ((and (is-ellipsis? pattern) (null? form)) '())
         ((member pattern literals)
          (if (eq? pattern (if (syntax-node? form) (syntax-node-sym form) form))
              '() #f))
         ((symbol? pattern)
          (list (make-match pattern (reverse raddr) sform)))
         ((and (pair? pattern) (not (pair? form))) #f)
         ((is-ellipsis? pattern)
          (apply merge-matches
                 (map (lambda (f n)
                        (inner (car pattern) f (cons n raddr)))
                      form
                      (iota (length form)))))
         ((pair? pattern)
          (merge-matches (inner (car pattern) (car form) raddr)
                         (inner (cdr pattern) (cdr form) raddr)))
         ((equal? pattern form) '())
         (else #f)))
      (inner pattern form '()))

    (define (is-subaddress subaddr fulladdr)
      (cond
       ((null? subaddr) #t)
       ((null? fulladdr) #f)
       ((not (equal? (car subaddr) (car fulladdr))) #f)
       (else
        (is-subaddress (cdr subaddr) (cdr fulladdr)))))

    (define (get-match name address matches)
      (cond
       ((null? matches) #f)
       ((and (eq? name (match-name (car matches)))
             (is-subaddress (match-address (car matches)) address))
        (car matches))
       (else
        (get-match name address (cdr matches)))))

    (define (all-zero lst)
      (cond
       ((null? lst) #t)
       ((not (zero? (car lst))) #f)
       (else (all-zero (cdr lst)))))

    (define (zero-past-first prefix addr)
      (all-zero (list-tail addr (+ 1 (length prefix)))))

    (define (get-matches name address matches)
      (cond
       ((null? matches) '())
       ((and (eq? name (match-name (car matches)))
             (is-subaddress address (match-address (car matches)))
             (zero-past-first address (match-address (car matches))))
        (cons (car matches)
              (get-matches name address (cdr matches))))
       (else
        (get-matches name address (cdr matches)))))

    (define (equalize-through-error a b msg)
      (cond
       ((not (and a b)) (or a b))
       ((equal? a b) a)
       (else (pscm-err msg))))

    (define (pattern-reps-top form addr matches)
      (cond
       ((is-ellipsis? form) #f)
       ((symbol? form)
        (let ((l (length (get-matches form addr matches))))
          (if (= l 0)
              #f
              l)))
       ((pair? form)
        (equalize-through-error
         (pattern-reps-top (car form) addr matches)
         (pattern-reps-top (cdr form) addr matches)
         "different lengths of ellipsis match in same expansion"))
       (else #f)))

    (define (fmax a b)
      (cond
       ((not a) b)
       ((not b) a)
       (else (max a b))))

    (define (pattern-reps-nested form addr matches)
      (cond
       ((is-ellipsis? form)
        (fmax (pattern-reps-nested (car form) addr matches)
              (pattern-reps-nested (cddr form) addr matches)))
       ((symbol? form)
        (let ((l (length (get-matches form addr matches))))
          (if (= l 0)
              #f
              l)))
       ((pair? form)
        (fmax
         (pattern-reps-nested (car form) addr matches)
         (pattern-reps-nested (cdr form) addr matches)))
       (else #f)))

    (define (pattern-reps form addr matches)
      (or (pattern-reps-top form addr matches)
          (pattern-reps-nested form addr matches)))

    (define-record-type syntax-node
      (make-syntax-node sym env instance)
      syntax-node?
      (sym syntax-node-sym)
      (env syntax-node-env)
      (instance syntax-node-instance))

    (define (syntax-equal? a b)
      (or (and (syntax-node? a) (syntax-node? b)
               (equal? (syntax-node-sym a) (syntax-node-sym b))
               (equal? (syntax-node-env a) (syntax-node-env b))
               (equal? (syntax-node-instance a) (syntax-node-instance b)))
          (equal? a b)))

    (define (apply-ellipsis-pattern matches form raddr env instance)
      (map
       (lambda (n) (apply-syntax-pattern matches form (cons n raddr) env instance))
       (iota (or (pattern-reps form (reverse raddr) matches) 0))))

    (define (apply-syntax-pattern matches form raddr env instance)
      (cond
       ((null? form) '())
       ((symbol? form)
        (cond
         ((get-match form (reverse raddr) matches) => match-form)
         ;; TODO: this is kind of a hack
         ((member form '(begin define-library import export define
                               define-syntax syntax-rules lambda if set!
                               quote builtin ffi-symbol))
          form)
         (else
          (make-syntax-node form env instance))))
       ((is-ellipsis? form)
        (append (apply-ellipsis-pattern matches (car form) raddr env instance)
                (apply-syntax-pattern matches (cddr form) raddr env instance)))
       ((pair? form)
        (cons (apply-syntax-pattern matches (car form) raddr env instance)
              (apply-syntax-pattern matches (cdr form) raddr env instance)))
       (else form)))

    (define-record-type syntax-rules
      (make-syntax-rules literals rules env)
      syntax-rules?
      (literals syntax-rules-literals)
      (rules syntax-rules-rules)
      (env syntax-rules-env))

    (define (rule-pattern rule)
      (car rule))

    (define (rule-transform rule)
      (cadr rule))

    ;; takes the cdr of a syntax-rules
    (define (transform-syntax-rules transformer args)
      (define (apply-inner rules)
        (define pattern (cdr (rule-pattern (car rules))))
        (define target (rule-transform (car rules)))
        (define matches (match-syntax-pattern pattern args (syntax-rules-literals transformer)))
        (cond
         (matches (apply-syntax-pattern matches target '() (syntax-rules-env transformer) (unique)))
         ((null? (cdr rules)) (pscm-err "no match for syntax-rule"))
         (else (apply-inner (cdr rules)))))
      (apply-inner (syntax-rules-rules transformer)))

    (define (transform-syntax transformer args)
      (cond
       ((syntax-rules? transformer) (transform-syntax-rules transformer args))
       (else (error "not a valid syntax transformer spec" transformer))))

    ))
