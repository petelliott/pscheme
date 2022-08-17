(define-library (pscheme compiler frontend)
  (import (scheme base)
          (scheme cxr)
          (pscheme compiler util)
          (pscheme compiler arch)
          (pscheme compiler library)
          (pscheme compiler syntax)
          (pscheme compiler languages)
          (pscheme compiler nanopass))
  (export frontend)
  (begin

    (define (do-import libs)
      (for-each compile-and-import libs))

    (define (macroexpand1 form)
      (apply-syntax-rules (lookup-syntax (car form)) form))

    (define-pass import-and-macroexpand (lscheme)
      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (define lib (new-library (name) () ()))
        (parameterize ((current-library lib))
          `(define-library ,(name) ,@(decls))))
       ((import ,@library-name) (names)
        (do-import (names))
        `(import ,@(names))))

      (library-declaration
       ((export ,@identifier) (names)
        (for-each (lambda (name)
                    (add-library-export! (current-library) name))
                  (names))
        `(export ,@(names)))
       ((import ,@library-name) (names)
        (do-import (names))
        `(import ,@(names))))

      (proc-toplevel
       ((define-syntax ,identifier ,any) (ident syntax)
        (add-library-syntax! (current-library) (ident) (cdr (syntax)))
        '(begin)))

      (expression
       ((,expression ,@expression) (name args)
        (define n (name))
        (if (lookup-syntax n)
            (import-and-macroexpand (macroexpand1 (cons n (args 'raw))))
            `(,n ,@(args))))))

    (define-pass normalize-forms (lscheme)
      ($ literal (l)
         `(quote ,l))
      (proc-toplevel
       ((define (,identifier ,@identifier) ,@proc-toplevel) (name args body)
        `(define ,(name) (lambda ,(args) ,@(body)))))
      (expression
       ((if ,expression ,expression) (test tbranch)
        `(if ,(test) ,(tbranch) (begin)))
       ((,expression ,@expression) (fn args)
        `(call ,(fn) ,@(args)))))

    (define-record-type frame
      (make-frame args rest-arg locals closure parent)
      frame?
      (args frame-args)
      (rest-arg frame-rest-arg set-frame-rest-arg!)
      (locals frame-locals set-frame-locals!)
      (closure frame-closure set-frame-closure!)
      (parent frame-parent))

    (define current-frame (make-parameter '()))

    (define (new-frame args rest-arg parent)
      (make-frame args rest-arg
                  (if rest-arg '((#f)) '())
                  '()
                  parent))

    (define (define-var! sym)
      (if (not (null? (current-frame)))
          (set-frame-locals! (current-frame) (cons sym (frame-locals (current-frame))))))

    (define (rest-arg arg-list)
      (cond
       ((symbol? arg-list) arg-list)
       ((null? arg-list) #f)
       ((pair? arg-list) (rest-arg (cdr arg-list)))
       (else (error "malformed argument list" arg-list))))

    (define (regular-args arg-list onto)
      (if (or (null? arg-list)
              (symbol? arg-list))
          onto
          (regular-args (cdr arg-list) (cons (car arg-list) onto))))

    (define (lookup-var-frame! sym frame)
      (cond
       ((null? frame)                     (lookup-global sym))
       ((eq? sym (frame-rest-arg frame))  '(stack 0))
       ((member sym (frame-locals frame)) `(stack ,(- (length (member sym (frame-locals frame))) 1)))
       ((member sym (frame-args frame))   `(arg ,(- (length (member sym (frame-args frame))) 1)))
       ((assq sym (frame-closure frame))  (error "this is fake")) ;(assq sym (frame-closure frame)))
       (else
        (let ((parent-ref (lookup-var-frame! sym (frame-parent frame))))
          (if (is-syntax? 'global parent-ref)
              parent-ref
              (let ((clos (member parent-ref (frame-closure frame))))
                (if clos
                    `(closure ,(- (length clos) 1))
                    (begin
                      (set-frame-closure! frame (cons parent-ref (frame-closure frame)))
                      `(closure ,(- (length (frame-closure frame)) 1))))))))))

    (define (lookup-var! sym)
      (lookup-var-frame! sym (current-frame)))

    (define-pass resolve-names (normal-scheme)
      ($ identifier (i)
         (lookup-var! i))

      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (parameterize ((current-library (lookup-library (name))))
          `(define-library ,(name) ,@(decls)))))

      (proc-toplevel
       ((define ,identifier ,expression) (name expr)
        (define-var! (name 'raw)) ; raw so the name is defined before we resolve it on the next line
        `(define ,(name) ,(expr))))

      (expression
       ((lambda (,@identifier) ,@proc-toplevel) (args body)
        (define arglist (args 'raw))
        (parameterize ((current-frame (new-frame (regular-args arglist '()) (rest-arg arglist) (current-frame))))
          (let* ((processed-body (body))
                 (nlocals (length (frame-locals (current-frame)))))
            `(closure
              (lambda ,arglist
                ,@(if (frame-rest-arg (current-frame))
                      `((accumulate-rest ,(length (frame-args (current-frame))) (stack 0)))
                      '())
                (push-locals ,nlocals)
                ,@processed-body)
              ,@(reverse (frame-closure (current-frame)))))))

       ((ffi-symbol ,symbol) (sym)
        `(ref (ffi ,(sym))))

       (,identifier (ident) `(ref ,(ident)))))


    (define (accumulate-library-decls forms imports exports begins)
      (if (null? forms)
          (values imports exports begins)
          (accumulate-library-decls (cdr forms)
                              (if (is-syntax? 'import (car forms)) (append imports (cdar forms)) imports)
                              (if (is-syntax? 'export (car forms)) (append exports (cdar forms)) exports)
                              (if (is-syntax? 'begin (car forms)) (append begins (cdar forms)) begins))))

    (define-pass to-backend-form (ref-scheme)
      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (define-values (imports exports begins) (accumulate-library-decls (decls) '() '() '()))
        `(define-library ,(name)
           ,@(map (lambda (imprt) `(import ,imprt)) imports)
           ,@(map (lambda (form) `(begin ,form)) begins)))

       ((import ,@library-name) (names)
        `(begin ,@(map (lambda (imprt) `(import ,imprt)) (names))))))


    (define frontend
      (concat-passes
       import-and-macroexpand
       normalize-forms
       resolve-names
       to-backend-form))

    ))
