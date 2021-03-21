(define-library (pscheme codegen)
  (import (scheme base)
          (scheme cxr)
          (pscheme arch)
          (pscheme util))
  (export codegen-main-file)

  (begin

    (define (codegen-main-file stmts)
      (enter-block-environment
       (lambda ()
         (emit 'c-prologue "main")
         (for-each codegen-stmt stmts)
         (emit 'c-epilogue))))

    (define (codegen-stmt stmt)
      (case (car stmt)
       ((define) (codegen-define stmt))
       ((push-locals)
        (when (> (cadr stmt) 0)
          (emit 'stack-alloc (cadr stmt))))
       (else (codegen-expr stmt))))

    (define (codegen-define stmt)
      (when (is-syntax? 'global (cadr stmt))
        (enter-block-environment (lambda ()
                                   (emit 'global-define-slot (cadadr stmt)))))
      (codegen-expr (caddr stmt))
      (emit 'mov 'result (cadr stmt)))

    ;; exprs always put their result in the 'result register
    (define (codegen-expr expr)
      (case (car expr)
        ((quote)   (codegen-literal (cadr expr)))
        ((ref)     (emit 'mov (cadr expr) 'result))
        ((lambda)  (codegen-lambda expr))
        ((call)    (codegen-call expr))
        ((if)      (codegen-if expr))
        ((closure) (codegen-expr (cadr expr))) ;; TODO: add closure support
        (else (error "unsuported expression " expr))))

    (define (codegen-literal literal)
      (cond
       ((integer? literal) (emit 'mov literal 'result))
       (else (error "can't generate code for literal: " literal))))

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

    ))
