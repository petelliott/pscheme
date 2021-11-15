(define-library (pscheme compiler frontend)
  (import (scheme base)
          (scheme cxr)
          (pscheme compiler util)
          (pscheme compiler arch)
          (pscheme compiler library)
          (pscheme compiler syntax))
  (export frontend)
  (begin

    (define-record-type frame
      (make-frame args locals closure parent)
      frame?
      (args frame-args)
      (locals frame-locals set-frame-locals!)
      (closure frame-closure set-frame-closure!)
      (parent frame-parent))

    (define (new-frame args parent)
      (make-frame args '() '() parent))

    (define (define-var! sym frame)
      (if (not (null? frame))
          (set-frame-locals! frame (cons sym (frame-locals frame)))))

    (define (lookup-var! sym frame)
      (cond
       ((null? frame)                     (lookup-global sym))
       ((member sym (frame-locals frame)) `(stack ,(- (length (member sym (frame-locals frame))) 1)))
       ((member sym (frame-args frame))   `(arg ,(- (length (member sym (frame-args frame))) 1)))
       ((assq sym (frame-closure frame))  (assq sym (frame-closure frame)))
       (else
        (let ((parent-ref (lookup-var! sym (frame-parent frame))))
          (if (is-syntax? 'global parent-ref)
              parent-ref
              (begin
                (set-frame-closure! frame (cons parent-ref (frame-closure frame)))
                `(closure ,(- (length (frame-closure frame)) 1))))))))

    (define (dump-frame frame)
      (if (null? frame)
          '()
          (cons
           (list (frame-args frame)
                 (frame-locals frame)
                 (frame-closure frame))
           (dump-frame (frame-parent frame)))))

    (define (is-macro? form)
      (and (pair? form)
           (lookup-syntax (car form))))

    ;; TODO: make macros sanitary.
    (define (macroexpand1 form)
      (apply-syntax-rules (lookup-syntax (car form)) form))

    ;;; the frontend makes all syntax explicit and figures out variable references and closures

    (define (frontend form)
      (frontend-toplevel form))

    (define (frontend-toplevel form)
      (cond
       ((is-macro? form) (frontend-toplevel (macroexpand1 form)))
       ((is-syntax? 'define-library form) (frontend-library form))
       ((is-syntax? 'import form) (frontend-import form))
       (else (frontend-stmt form '()))))

    (define (accumulate-library-decls forms imports exports begins)
      (if (null? forms)
          (values imports exports begins)
          (accumulate-library-decls (cdr forms)
                              (if (is-syntax? 'import (car forms)) (append imports (cdar forms)) imports)
                              (if (is-syntax? 'export (car forms)) (append exports (cdar forms)) exports)
                              (if (is-syntax? 'begin (car forms)) (append begins (cdar forms)) begins))))

    (define (frontend-library form)
      (define name (cadr form))
      (define-values (imports exports begins) (accumulate-library-decls (cddr form) '() '() '()))
      (define lib (new-library name () exports))
      (parameterize ((current-library lib))
        (for-each compile-and-import imports)
        `(define-library ,name
           ,@(map (lambda (imprt) `(import ,imprt)) imports)
           ,@(map (lambda (form) (frontend-stmt form '())) begins))))

    (define (frontend-import form)
      `(begin
         ,@(map (lambda (imprt)
                  (compile-and-import imprt)
                  `(import ,imprt))
                (cdr form))))

    (define (frontend-stmt form scope)
      (cond
       ((is-macro? form) (frontend-stmt (macroexpand1 form)))
       ((is-syntax? 'define form)
        (frontend-define form scope))
       ((is-syntax? 'begin form) `(begin ,@(map (lambda (form) (frontend-stmt form scope)) (cdr form))))
       ((is-syntax? 'define-syntax form) (frontend-define-syntax form scope))
       (else (frontend-expr form scope))))

    (define (frontend-define form scope)
      (cond
       ((pair? (cadr form))
        (frontend-define `(define ,(caadr form) (lambda ,(cdadr form) ,@(cddr form))) scope))
       (else
        (define-var! (cadr form) scope)
        `(define ,(lookup-var! (cadr form) scope) ,(frontend-expr (caddr form) scope)))))

    (define (frontend-define-syntax form scope)
      (add-library-syntax! (current-library) (cadr form) (frontend-syntax (caddr form) scope))
      '(begin))

    (define (frontend-syntax form scope)
      (cond
       ((is-syntax? 'syntax-rules form) (cdr form))
       (else (error "not a valid syntax form" form))))

    (define (frontend-expr form scope)
      (cond
       ((is-macro? form) (frontend-expr (macroexpand1 form)))
       ((is-syntax? 'quote form) form)
       ((is-syntax? 'begin form) `(begin ,@(map (lambda (form) (frontend-expr form scope)) (cdr form))))
       ((is-syntax? 'define form) (error "define in expression context"))
       ((is-syntax? 'builtin form)
        `(builtin ,(cadr form)
                  ,@ (map (lambda (form) (frontend-expr form scope)) (cddr form))))
       ((is-syntax? 'if form)
        `(if ,(frontend-expr (cadr form) scope)
             ,(frontend-expr (caddr form) scope)
             ,(frontend-expr (cadddr form) scope)))
       ((is-syntax? 'set! form)
        `(set! ,(lookup-var! (cadr form)) ,(frontend-expr (caddr form) scope)))
       ((is-syntax? 'lambda form) (frontend-lambda form scope))
       ((pair? form) (cons 'call (map (lambda (form) (frontend-expr form scope)) form)))
       ((symbol? form) `(ref ,(lookup-var! form scope)))
       (else `(quote ,form))))

    (define (frontend-lambda form scope)
      (define new-scope (new-frame (reverse (cadr form)) scope))
      (define body (map (lambda (form) (frontend-stmt form new-scope)) (cddr form)))
      `(closure
        (lambda ,(cadr form)
          (push-locals ,(length (frame-locals new-scope)))
          ,@body)
        ,@(reverse (frame-closure new-scope))))

    ))
