(define-library (pscheme compiler middleend)
  (import (scheme base)
          (srfi 1)
          (pscheme base)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler file)
          (pscheme compiler languages)
          (pscheme compiler nanopass))
  (export middleend)
  (begin

    ;;; stuff for unpacking trees

    (define lb-param (make-parameter '()))

    (define (call-with-list-block proc)
      (parameterize ((lb-param '()))
        (proc)
        (reverse (lb-param))))

    (define-syntax with-list-block
      (syntax-rules ()
        ((_ body ...)
         (call-with-list-block (lambda () body ...)))))

    (define tl-param (make-parameter '()))

    (define (call-with-toplevel proc)
      (parameterize ((lb-param '()))
        (proc)
        (tl-param (append (tl-param) (reverse (lb-param))))))

    (define-syntax with-toplevel
      (syntax-rules ()
        ((_ body ...)
         (call-with-toplevel (lambda () body ...)))))

    (define (emit form)
      (define sform
        (if (current-span)
            (copy-span (current-span) form)
            form))
      (lb-param (cons sform (lb-param))))

    ;;; conversion from tail-ref-scheme to ir

    (define (normal-args args)
      (if (pair? args)
          (cons (unbox (car args))
                (normal-args (cdr args)))
          '()))

    (define (rest-arg args)
      (cond
       ((pair? args) (rest-arg (cdr args)))
       ((box? args) (unbox args))
       ((null? args) #f)))

    (define (emit-tmp-op op)
      (define tmp `(tmp ,(unique)))
      (emit `(,tmp ,op))
      tmp)

    (define (literal value)
      (cond
       ((string? value)
        (let ((name `(data string string ,(unique))))
          (with-toplevel (emit `(data ,name ,value)))
          name))
       ((symbol? value)
        (let ((name `(data symbol symbol ,value)))
          (with-toplevel (emit `(data ,name ,(symbol->string value))))
          name))
       ((pair? value)
        (let ((name `(data pair pair ,(unique))))
          (with-toplevel (emit `(data ,name ,(literal (car value)) ,(literal (cdr value)))))
          name))
       (else value)))

    (define (ident->reg ident)
      (define nident (strip-spans ident))
      (cond
       ((is-syntax? 'closure nident)
        (emit-tmp-op `(closure-ref ,ident)))
       ((is-syntax? 'global nident)
        (emit-tmp-op `(global-ref ,ident)))
       (else
        ident)))

    (define-pass irconvert1 (tail-ref-scheme)
      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (emit `(entry ,(name) ,@(with-list-block (decls)))))
       ((import ,@library-name) (names)
        (for-each (lambda (name)
                    (emit `(void (import ,name))))
                  (names 'raw))))

      (library-declaration
       ((import ,@library-name) (names)
        (for-each (lambda (name)
                    (emit `(void (import ,name))))
                  (names 'raw))))

      (proc-toplevel
       ((begin ,@proc-toplevel) (stmts)
        (if (null? (stmts 'raw))
            '(unspecified)
            (last (stmts))))
       ((accumulate-rest ,number) (n)
        (emit `(void (accumulate-rest ,(n)))))
       ((define ,identifier ,expression) (ident expr)
        (if (is-syntax? 'global (ident 'raw))
            (begin
              (with-toplevel (emit `(define ,@(cdr (ident 'raw)))))
              (emit `(void (global-set! ,(ident) ,(expr)))))
            (begin
              (emit `(void (meta-define ,(ident))))
              (emit `(void (set! ,(ident) ,(expr))))))))

      (lambda-name
       ((anon) ()
        `(data none lambda ,(unique)))
       (,symbol (sym)
        `(data none lambda ,(sym) ,(unique))))

      (expression
       ((begin ,@expression) (stmts)
        (if (null? (stmts 'raw))
            '(unspecified)
            (last (stmts))))
       ((lambda ,lambda-name (,@box) ,@proc-toplevel) (lname args body)
        (define name (lname))
        (with-toplevel
         (emit `(lambda ,name ,(normal-args (args 'raw)) ,(rest-arg (args 'raw))
                        ,@(with-list-block (emit `(void (return ,(last (body)))))))))
        `(quote ,name))
       ((if ,expression ,expression ,expression) (c tbranch fbranch)
        (define treg #f)
        (define tinsts (with-list-block (set! treg (tbranch))))
        (define freg #f)
        (define finsts (with-list-block (set! freg (fbranch))))
        (emit-tmp-op `(if ,(c) ,treg ,tinsts ,freg ,finsts)))
       ((set! ,identifier ,expression) (ident expr)
        (if (is-syntax? 'global (ident 'raw))
            (emit-tmp-op `(global-set! ,(ident) ,(expr)))
            (emit-tmp-op `(set! ,(ident) ,(expr)))))
       ((quote ,any) (val)
        `(quote ,(literal (val 'raw))))
       ((builtin ,symbol ,@expression) (sym args)
        (emit-tmp-op `(builtin ,(sym) ,@(args))))
       ((ref ,identifier) (ident)
        (ident->reg (ident)))
       ((closure ,expression ,@identifier) (expr idents)
        (emit-tmp-op `(closure ,(expr) ,@(map ident->reg (idents)))))
       ((call ,bool ,expression ,@expression) (tail fn args)
        (emit-tmp-op `(call ,(tail) ,(fn) ,@(args))))))

    (define (irconvert rs program-or-lib)
      (parameterize ((tl-param '()))
        (with-toplevel
         (case program-or-lib
           ((program)
            (emit `(entry main ,@(with-list-block (for-each irconvert1 rs)))))
           ((library)
            (for-each irconvert1 rs))
           (else (error "invalid argument to irconvert: " program-or-lib))))
        (tl-param)))


    (define renames (make-parameter '()))
    (define (rename-reg from to)
      (renames (cons (cons from to)
                     (renames))))

    (define (nameof name names)
      (or (assoc-ref name names)
          name))

    (define (cdr-member rest lst)
      (cond
       ((eq? rest lst) lst)
       ((null? lst) #f)
       (else (cdr-member rest (cdr lst)))))

    (define (common-root a b)
      (cond
       ((cdr-member a b) => (lambda (a) a))
       ((null? a) '())
       (else (common-root (cdr a) b))))

    (define (since-root l root)
      (cond
       ((eq? l root) '())
       ((null? l) '())
       (else (cons (car l)
                   (since-root (cdr l) root)))))

    (define (generate-phis ra rb)
      (define root (common-root ra rb))
      (define sra (since-root ra root))
      (define srb (since-root rb root))
      (map (lambda (name)
             (define new-name `(tmp ,(unique)))
             (rename-reg name new-name)
             `(phi ,name ,new-name ,(nameof name ra) ,(nameof name rb)))
           (delete-duplicates (map car (append sra srb)))))

    (define-pass ssa-convert (ir)
      (program
       ((,@toplevel-def) (defs)
        (parameterize ((renames '()))
          (defs))))
      (op
       ((set! ,identifier ,value) (lval rval)
        (define l (lval 'raw))
        (define r (rval))
        (rename-reg l r)
        `(meta-set! ,l ,r))
       ((if ,value ,value (,@instruction) ,value (,@instruction)) (con tphi tbranch fphi fbranch)
        (define tb #f)
        (define tr #f)
        (define fb #f)
        (define fr #f)
        (parameterize ((renames (renames)))
          (set! tb (tbranch))
          (set! tr (renames)))
        (parameterize ((renames (renames)))
          (set! fb (fbranch))
          (set! fr (renames)))
        `(if ,(con) ,(tphi) ,tb ,(fphi) ,fb ,(generate-phis tr fr))))
      (identifier
       ((local ,number ,var-metadata) (n v)
        (nameof `(local ,(n 'raw) ,(v 'raw)) (renames)))
       ((arg rest ,var-metadata) (v)
        (nameof `(arg rest ,(v 'raw)) (renames)))
       ((arg ,number ,var-metadata) (n v)
        (nameof `(arg ,(n 'raw) ,(v 'raw)) (renames)))))

    (define (middleend prog program-or-lib)
      (define inner
        (concat-passes
         (lambda (p) (irconvert p program-or-lib))
         ssa-convert))
      (parameterize ((current-unique 0))
        (inner prog)))

    ))
