(define-library (pscheme compiler codegen)
  (import (scheme base)
          (scheme cxr)
          (pscheme compiler arch)
          (pscheme compiler util))
  (export codegen-main-file
          codegen-library-file)
  (begin

    (define (codegen-main-file stmts)
      (enter-block-environment
       (lambda ()
         (emit 'c-prologue "main")
         (for-each codegen-stmt stmts)
         (emit 'c-epilogue)
         (emit 'file-extras))))

    (define (codegen-library-file stmts)
      (for-each codegen-library-toplevel stmts)
      (emit 'file-extras))

    (define (codegen-library-toplevel stmt)
      (case (car stmt)
        ((define-library) (codegen-define-library stmt))
        (else (error "unexpected statement in library file context:" stmt))))

    (define (library-entry lib)
      (string-append "pscm_entry_" (mangle-library lib)))

    (define (codegen-define-library form)
      (enter-block-environment
       (lambda ()
         (emit 'c-prologue (library-entry (cadr form)))
         (for-each (lambda (decl)
                     (case (car decl)
                       ((import) (codegen-import decl))
                       ((begin) (for-each codegen-stmt (cdr decl)))))
                   (cddr form))
         (emit 'c-epilogue)))
      'unspecified)

    (define (codegen-block f subexprs)
      (if (null? subexprs)
          'unspecified
          (let* ((reversed (reverse subexprs))
                 (last (car reversed))
                 (notlast (reverse (cdr reversed))))
            (for-each f notlast)
            (f last))))

    (define (codegen-stmt stmt)
      (case (car stmt)
        ((define) (codegen-define stmt))
        ((import) (codegen-import stmt))
        ((push-locals) (codegen-push-locals stmt))
        ((accumulate-rest) (codegen-accumulate-rest stmt))
        ((begin) (codegen-block codegen-stmt (cdr stmt)))
        (else (codegen-expr stmt))))

    (define (codegen-define stmt)
      (when (is-syntax? 'global (cadr stmt))
        (enter-block-environment (lambda ()
                                   (emit 'global-define-slot (cadadr stmt) (car (cddadr stmt))))))
      (emit 'mov (codegen-expr (caddr stmt)) (cadr stmt))
      'unspecified)

    (define (codegen-import stmt)
      (for-each (lambda (form)
                  (emit 'c-call (library-entry form)))
                (cdr stmt))
      'unspecified)

    (define (codegen-push-locals stmt)
      (when (> (cadr stmt) 0)
        (emit 'stack-alloc (cadr stmt)))
      'unspecified)

    (define (codegen-accumulate-rest stmt)
      (emit 'accumulate-rest (cadr stmt) (caddr stmt))
      'unspecified)

    ;; exprs return a ref-accessor to their result
    (define (codegen-expr expr)
      (case (car expr)
        ((quote)   (codegen-literal (cadr expr)))
        ((ref)     (codegen-ref expr))
        ((lambda)  (codegen-lambda expr))
        ((call)    (codegen-call expr))
        ((if)      (codegen-if expr))
        ((closure) (codegen-closure expr))
        ((begin)   (codegen-block codegen-expr (cdr expr)))
        ((builtin) (codegen-builtin (cadr expr) (cddr expr)))
        ((set!)    (codegen-set (cadr expr) (caddr expr)))
        (else (error "unsuported expression " expr))))

    (define (codegen-literal literal)
      (cond
       ((integer? literal)
        `(immediate ,(emit-eval 'fixnum-literal literal)))
       ((member literal '(() #f #t))
        `(immediate ,(emit-eval 'singleton-literal literal)))
       ((pair? literal)
        (let ((left (codegen-literal (car literal)))
              (right (codegen-literal (cdr literal)))
              (label (genlabel "pscm_cons")))
          (enter-block-environment
           (lambda () (emit 'cons-literal label left right)))
          `(data ,(emit-eval 'tag-label label 'pair))))
       ((char? literal)
        `(immediate ,(emit-eval 'char-literal literal)))
       ((string? literal)
        (let ((label (genlabel "pscm_string")))
          (enter-block-environment
           (lambda () (emit 'string-literal label literal)))
          `(data ,(emit-eval 'tag-label label 'string))))
       (else (error "can't generate code for literal: " literal))))

    (define (codegen-ref expr)
      (cadr expr))

    (define (codegen-lambda expr)
      (define label (genlabel "pscm_lambda"))
      (enter-block-environment
       (lambda ()
         (emit 'prologue label)
         (emit 'mov (codegen-block codegen-stmt (cddr expr)) 'result)
         (emit 'epilogue)))
      `(data ,label))

    (define (codegen-call expr)
      (emit 'prepare (codegen-expr (cadr expr)))
      (for-each (lambda (expr)
                  (emit 'pusharg (codegen-expr expr)))
                (cddr expr))
      (emit 'call-closure (length (cddr expr)))
      'result)

    (define (codegen-if expr)
      (define label (genlabel ""))
      (emit 'if-prologue (codegen-expr (cadr expr)) label)
      (emit 'mov (codegen-expr (caddr expr)) 'result)
      (emit 'if-middle label)
      (if (null? (cdddr expr))
          (emit 'mov 'unspecified 'result)
          (emit 'mov (codegen-expr (cadddr expr)) 'result))
      (emit 'if-end label)
      'result)

    (define (codegen-closure expr)
      (emit 'enclose (codegen-expr (cadr expr)) (cddr expr))
      'result)

    (define (codegen-builtin builtin args)
      (define refs (map codegen-expr args))
      (for-each (lambda (ref)
                  (when (eq? 'result ref)
                    (error "only the last arg of a builtin can be 'result"
                           `(builtin ,builtin ,@args))))
                (cdr (reverse refs)))

      (emit 'builtin builtin refs)
      'result)

    (define (codegen-set ref value)
      (emit 'mov (codegen-expr value) ref)
      'unspecified)

    ))
