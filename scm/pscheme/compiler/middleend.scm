(define-library (pscheme compiler middleend)
  (import (scheme base)
          (srfi 1)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler languages)
          (pscheme compiler nanopass))
  (export middleend)
  (begin

    ;;; utils

    (define (make-unique)
      (define n 0)
      (lambda ()
        (set! n (+ n 1))
        n))

    (define unique (make-parameter (make-unique)))

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
      (lb-param (cons form (lb-param))))

    ;;; conversion from ref-scheme to ir

    (define (rm-vm arg)
      (match arg
       ((stack ,number ,var-metadata)
        `(local ,number))
       ((arg ,number ,var-metadata)
        `(arg ,number))
       ((closure ,number ,var-metadata)
        `(closure ,number))
       (else
        arg)))


    (define (normal-args args)
      (if (pair? args)
          (cons (rm-vm (unbox (car args)))
                (normal-args (cdr args)))
          '()))

    (define (rest-arg args)
      (cond
       ((pair? args) (rest-arg (cdr args)))
       ((box? args) (rm-vm (unbox args)))
       ((null? args) #f)))

    (define (emit-tmp-op op)
      (define tmp `(tmp ,((unique))))
      (emit `(,tmp ,op))
      tmp)

    (define (literal value)
      (cond
       ((string? value)
        (let ((name `(data string string ,((unique)))))
          (with-toplevel (emit `(data ,name ,value)))
          name))
       ((symbol? value)
        (let ((name `(data symbol symbol ,((unique)))))
          (with-toplevel (emit `(data ,name ,(symbol->string value))))
          name))
       ((pair? value)
        (let ((name `(data pair pair ,((unique)))))
          (with-toplevel (emit `(data ,name ,(literal (car value)) ,(literal (cdr value)))))
          name))
       (else value)))

    (define-pass irconvert1 (ref-scheme)
      (identifier
       ((stack ,number ,var-metadata) (n v)
        `(local ,(n)))
       ((arg ,number ,var-metadata) (n v)
        `(arg ,(n)))
       ((closure ,number ,var-metadata) (n v)
        `(closure ,(n))))

      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (emit `(entry ,(name) ,@(with-list-block (decls)))))
       ((import ,@library-name) (names)
        (for-each (lambda (name)
                    (emit `(void (import ,name))))
                  (names 'raw))))

      (proc-toplevel
       ((begin ,@proc-toplevel) (stmts)
        (if (null? (stmts 'raw))
            (emit-tmp-op '(load-special undefined))
            (last (stmts))))
       ((define ,identifier ,expression) (ident expr)
        (if (is-syntax? 'global (ident 'raw))
            (with-toplevel (emit `(define ,@(cdr (ident 'raw))))))
        (emit `(void (set! ,(ident) ,(expr))))))

      (expression
       ((begin ,@expression) (stmts)
        (if (null? (stmts 'raw))
            (emit-tmp-op '(load-special undefined))
            (last (stmts))))
       ((lambda (,@box) ,@proc-toplevel) (args body)
        (define name `(data none lambda ,((unique))))
        (with-toplevel
         (emit `(lambda ,name ,(normal-args (args 'raw)) ,(rest-arg (args 'raw))
                        ,@(with-list-block (emit `(void (return ,(last (body)))))))))
        (emit-tmp-op `(tag-data ,name)))
       ((if ,expression ,expression ,expression) (c tbranch fbranch)
        (define treg #f)
        (define tinsts (with-list-block (set! treg (tbranch))))
        (define freg #f)
        (define finsts (with-list-block (set! freg (fbranch))))
        (emit-tmp-op `(if ,(c) ,treg ,tinsts ,freg ,finsts)))
       ((set! ,identifier ,expression) (ident expr)
        (emit-tmp-op `(set! ,(ident) ,(expr))))
       ((quote ,any) (val)
        (define data (literal (val 'raw)))
        (emit-tmp-op
         (if (is-syntax? 'data data)
             `(tag-data ,data)
             `(load-imm ,data))))
       ((builtin ,symbol ,@expression) (sym args)
        (emit-tmp-op `(builtin ,(sym) ,@(args))))
       ((ref ,identifier) (ident)
        (if (is-syntax? 'closure (ident 'raw))
            (emit-tmp-op `(closure-ref ,(ident)))
            (ident)))
       ((closure ,expression ,@identifier) (expr idents)
        (emit-tmp-op `(closure ,(expr) ,@(idents))))
       ((call ,expression ,@expression) (fn args)
        (emit-tmp-op `(call ,(fn) ,@(args))))))

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

    (define (middleend prog program-or-lib)
      (define inner
        (concat-passes
         (lambda (p) (irconvert p program-or-lib))))
      (parameterize ((unique (make-unique)))
        (inner prog)))

    ))
