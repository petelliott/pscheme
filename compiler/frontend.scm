(define-library (pscheme frontend)
  (import (scheme base)
          (scheme cxr)
          (pscheme util))
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
       ((null? frame) `(global ,sym))
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

    ;;; stage0 makes all syntax explicit and figures out variable references and closures

    (define (stage0 form)
      (stage0-stmt form '()))

    (define (stage0-stmt form scope)
      (cond
       ((is-syntax? 'define form)
        (stage0-define form scope))
       ((is-syntax? 'begin form) `(begin ,@(map (lambda (form) (stage0-stmt form scope)) (cdr form))))
       (else (stage0-expr form scope))))

    (define (stage0-define form scope)
      (cond
       ((pair? (cadr form))
        (stage0-define `(define ,(caadr form) (lambda ,(cdadr form) ,@(cddr form))) scope))
       (else
        (define-var! (cadr form) scope)
        `(define ,(lookup-var! (cadr form) scope) ,(stage0-expr (caddr form) scope)))))

    (define (stage0-expr form scope)
      (cond
       ((is-syntax? 'quote form) form)
       ((is-syntax? 'begin form) `(begin ,@(map (lambda (form) (stage0-expr form scope)) (cdr form))))
       ((is-syntax? 'define form) (error "define in expression context"))
       ((is-syntax? 'if form)
        `(if ,(stage0-expr (cadr form) scope)
             ,(stage0-expr (caddr form) scope)
             ,(stage0-expr (cadddr form) scope)))
       ((is-syntax? 'set! form)
        `(set! ,(lookup-var! (cadr form)) ,(stage0-expr (caddr form) scope)))
       ((is-syntax? 'lambda form) (stage0-lambda form scope))
       ((pair? form) (cons 'call (map (lambda (form) (stage0-expr form scope)) form)))
       ((symbol? form) `(ref ,(lookup-var! form scope)))
       (else `(quote ,form))))

    (define (stage0-lambda form scope)
      (define new-scope (new-frame (reverse (cadr form)) scope))
      (define body (map (lambda (form) (stage0-stmt form new-scope)) (cddr form)))
      `(closure
        (lambda ,(cadr form)
          (push-locals ,(length (frame-locals new-scope)))
          ,@body)
        ,@(reverse (frame-closure new-scope))))

    (define (frontend form)
      (stage0 form))

    ))
