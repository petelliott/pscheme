(define-library (pscheme compiler codegen)
  (import (scheme base)
          (scheme cxr)
          (scheme write)
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
         (emit 'c-epilogue))))

    (define (codegen-library-file stmts)
      (for-each codegen-library-toplevel stmts))

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
         (for-each codegen-stmt (cddr form))
         (emit 'c-epilogue))))

    (define (codegen-stmt stmt)
      (case (car stmt)
       ((define) (codegen-define stmt))
       ((import) (codegen-import stmt))
       ((push-locals)
        (when (> (cadr stmt) 0)
          (emit 'stack-alloc (cadr stmt))))
       ((begin) (for-each codegen-stmt (cdr stmt)))
       (else (codegen-expr stmt))))

    (define (codegen-define stmt)
      (when (is-syntax? 'global (cadr stmt))
        (enter-block-environment (lambda ()
                                   (emit 'global-define-slot (cadadr stmt) (car (cddadr stmt))))))
      (codegen-expr (caddr stmt))
      (emit 'mov 'result (cadr stmt)))

    (define (codegen-import stmt)
      (emit 'c-call (library-entry (cadr stmt))))

    ;; exprs always put their result in the 'result register
    (define (codegen-expr expr)
      (case (car expr)
        ((quote)   (codegen-literal (cadr expr)))
        ((ref)     (emit 'mov (cadr expr) 'result))
        ((lambda)  (codegen-lambda expr))
        ((call)    (codegen-call expr))
        ((if)      (codegen-if expr))
        ((closure) (codegen-expr (cadr expr))) ;; TODO: add closure support
        ((begin)   (for-each codegen-expr (cdr expr)))
        ((builtin) (codegen-builtin (cadr expr) (cddr expr)))
        (else (error "unsuported expression " expr))))

    (define (codegen-literal-inner literal)
      (cond
       ((integer? literal) (emit-eval 'fixnum-literal literal))
       ((member literal '(() #f #t)) (emit-eval 'singleton-literal literal))
       ((pair? literal)
        (let ((left (codegen-literal-inner (car literal)))
              (right (codegen-literal-inner (cdr literal)))
              (label (genlabel "pscm_cons")))
          (enter-block-environment
           (lambda () (emit 'cons-literal label left right)))
          (emit-eval 'tag-cons-label label)))
       (else (error "can't generate code for literal: " literal))))

    (define (codegen-literal literal)
      (cond
       ((pair? literal) (emit 'load-data-literal (codegen-literal-inner literal)))
       (else (emit 'load-immediate-literal (codegen-literal-inner literal)))))

    (define (codegen-lambda expr)
      (define label (genlabel "pscm_lambda"))
      (enter-block-environment
       (lambda ()
         (emit 'prologue label)
         (for-each codegen-stmt (cddr expr))
         (emit 'epilogue)))
      (emit 'load-lambda label))

    (define (codegen-call expr)
      (codegen-expr (cadr expr))
      (emit 'prepare)
      (for-each (lambda (expr)
                  (codegen-expr expr)
                  (emit 'pusharg))
                (cddr expr))
      (emit 'call (length (cddr expr))))

    (define (codegen-if expr)
      (define label (genlabel ""))
      (codegen-expr (cadr expr))
      (emit 'if-prologue label)
      (codegen-expr (caddr expr))
      (emit 'if-middle label)
      (codegen-expr (cadddr expr))
      (emit 'if-end label))

    (define (codegen-builtin builtin args)
      (for-each (lambda (expr)
                  (codegen-expr expr)
                  (emit 'push-builtin-arg))
                (cddr expr))
      (emit 'builtin builtin args)
      (emit 'pop-builtin-args (length args)))

    ))
