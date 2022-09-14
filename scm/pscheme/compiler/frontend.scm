(define-library (pscheme compiler frontend)
  (import (scheme base)
          (scheme cxr)
          (srfi 1)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler library)
          (pscheme compiler syntax)
          (pscheme compiler file)
          (pscheme compiler languages)
          (pscheme compiler error)
          (pscheme compiler nanopass))
  (export frontend)
  (begin

    (define (do-import libs)
      (for-each (lambda (lib)
                  (unless (library-filename (strip-spans lib))
                    (if (span? lib)
                        (pscm-err lib "can't find library ~a" (strip-spans lib)))
                        (pscm-err "can't find library ~a" lib))
                  (compile-and-import (strip-spans lib)))
                libs))

    (define (macroexpand1 form)
      (apply-syntax-rules (lookup-syntax (car form)) form))

    (define-pass import-and-macroexpand (lscheme)
      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (define lib (new-library (name 'raw) () ()))
        (parameterize ((current-library lib))
          `(define-library ,(name) ,@(decls))))
       ((import ,@library-name) (names)
        (do-import (names 'span))
        `(import ,@(names))))

      (library-declaration
       ((export ,@identifier) (names)
        (for-each (lambda (name)
                    (add-library-export! (current-library) name))
                  (names 'raw))
        '(begin))
       ((import ,@library-name) (names)
        (do-import (names 'span))
        `(import ,@(names))))

      (proc-toplevel
       ((define-syntax ,identifier ,any) (ident syntax)
        (add-library-syntax! (current-library) (ident 'raw) (cdr (syntax 'raw)))
        '(begin)))

      (expression
       ((,expression ,@expression) (name args)
        (define n (name 'raw))
        (if (lookup-syntax n)
            (import-and-macroexpand (macroexpand1 (cons n (args 'raw))))
            `(,(name) ,@(args))))))

    (define-pass track-defines (lscheme)
      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (parameterize ((current-library (lookup-library (name 'raw))))
          `(define-library ,(name) ,@(decls)))))

      (proc-toplevel
       ((define ,identifier ,expression) (ident expr)
        (add-library-define! (current-library) (ident 'raw) #f)
        `(define ,(ident) ,(expr 'span)))
       ((define (,identifier ,@identifier) ,@proc-toplevel) (ident args body)
        (add-library-define! (current-library) (ident 'raw) (args 'raw))
        `(define (,(ident) ,@(args)) ,@(body 'span)))
       (,expression (expr) (expr 'span))))

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
          (set-frame-locals! (current-frame) (cons (make-var-metadata sym #f #f)
                                                   (frame-locals (current-frame))))))

    (define (rest-arg arg-list)
      (cond
       ((symbol? arg-list) (make-var-metadata arg-list #f #f))
       ((null? arg-list) #f)
       ((pair? arg-list) (rest-arg (cdr arg-list)))
       (else (error "malformed argument list" arg-list))))

    (define (regular-args arg-list onto)
      (if (or (null? arg-list)
              (symbol? arg-list))
          onto
          (regular-args (cdr arg-list)
                        (cons (make-var-metadata (car arg-list) #f #f) onto))))

    (define (vm-cmp a b)
      (equal? (if (var-metadata? a) (vm-sym a) a)
              (if (var-metadata? b) (vm-sym b) b)))

    (define (lookup-var-frame! sym frame)
      (cond
       ((null? frame)
        (or (lookup-global sym)
            (pscm-err "undefined variable ~a" sym)))
       ((and (frame-rest-arg frame)
             (eq? sym (vm-sym (frame-rest-arg frame))))
        `(stack 0 ,(frame-rest-arg frame)))
       ((member sym (frame-locals frame) vm-cmp) =>
        (lambda (m)
          `(stack ,(- (length m) 1) ,(car m))))
       ((member sym (frame-args frame) vm-cmp)  =>
        (lambda (m)
          `(arg ,(- (length m) 1) ,(car m))))
       (else
        (let ((parent-ref (lookup-var-frame! sym (frame-parent frame))))
          (if (is-syntax? 'global parent-ref)
              parent-ref
              (let ((clos (member parent-ref (frame-closure frame) vm-cmp)))
                (if clos
                    `(closure ,(- (length clos) 1) ,(last (car clos)))
                    (begin
                      (set-frame-closure! frame (cons parent-ref (frame-closure frame)))
                      `(closure ,(- (length (frame-closure frame)) 1)
                                ,(last (car (frame-closure frame))))))))))))

    (define (lookup-var! sym)
      (lookup-var-frame! sym (current-frame)))

    (define-pass resolve-names (normal-scheme)
      ($ identifier (i)
         (lookup-var! i))

      (program-toplevel
       ((define-library ,library-name ,@library-declaration) (name decls)
        (parameterize ((current-library (lookup-library (name 'raw))))
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
              (lambda ,(sloppy-map (lambda (var) (make-box (lookup-var! var))) arglist)
                (begin
                  ,@(if (frame-rest-arg (current-frame))
                        `((accumulate-rest ,(length (frame-args (current-frame))) (stack 0)))
                        '())
                  (push-locals ,nlocals))
                ,@processed-body)
              ,@(reverse (frame-closure (current-frame)))))))

       ((ffi-symbol ,symbol) (sym)
        `(ref (ffi ,(sym))))

       (,identifier (ident) `(ref ,(ident)))))

    (define (arg-match args shape)
      (cond
       ((and (null? args) (null? shape))
        #t)
       ((and (pair? args) (pair? shape))
        (arg-match (cdr args) (cdr shape)))
       ((symbol? shape) #t)
       ((null? args) #f)
       ((null? shape) #f)))

    (define-pass check-args (ref-scheme)
      (expression
       ((call ,expression ,@expression) (f args)
        (match (f 'raw)
          ((ref (global ,lname ,sym))
           (let* ((shape (assoc sym (library-defines (lookup-library lname))))
                  (shape (and shape (cdr shape))))
             (when (and shape (not (arg-match (args 'raw) shape)))
               (pscm-warn "wrong number of arguments to function ~a" (cons sym shape))))))
        `(call ,(f) ,@(args)))))

    (define in-var (make-parameter #f))

    (define-pass flag-sets (ref-scheme)
      (proc-toplevel
       ((define ,identifier ,expression) (ident expr)
        (parameterize ((in-var (and (var-metadata? (last (ident 'raw)))
                                    (last (ident 'raw)))))
          `(define ,(ident) ,(expr)))))

      (expression
       ((set! ,identifier ,expression) (i e)
        (if (var-metadata? (last (i 'raw)))
            (vm-set-ever-set! (last (i 'raw)) #t))
        `(set! ,(i) ,(e)))
       ((closure ,expression ,@identifier) (expr idents)
        (for-each (lambda (i)
                    (when (var-metadata? (last i))
                      (vm-set-ever-enclosed! (last i) #t)
                      (when (and (in-var)
                                 (eq? (in-var) (last i)))
                        (vm-set-ever-set! (last i) #t))))
                  (idents 'raw))
        `(closure ,(expr) ,@(idents)))))

    (define (should-box ref)
      (and (var-metadata? (last ref))
           (vm-ever-set? (last ref))
           (vm-ever-enclosed? (last ref))))

    (define (improper->proper lst)
      (cond
       ((null? lst) lst)
       ((pair? lst)
        (cons (car lst)
              (improper->proper (cdr lst))))
       (else
        (list lst))))

    (define-pass box-sets (ref-scheme)
      (proc-toplevel
       ((define ,identifier ,expression) (ident expr)
        (if (should-box (ident 'raw))
            `(begin
               (define ,(ident) (builtin cons '#f '#f))
               (builtin set-cdr! (ref ,(ident)) ,(expr)))
            `(define ,(ident) ,(expr)))))

      (expression
       ((set! ,identifier ,expression) (ident expr)
        (if (should-box (ident 'raw))
            `(builtin set-cdr! (ref ,(ident)) ,(expr))
            `(set! ,(ident) ,(expr))))

       ((lambda (,@box) ,@proc-toplevel) (args body)
        (define b (body))
        `(lambda (,@(args))
           ,(car b) ;; make sure we accumulate the rest arguments before boxing them
           ,@(map (lambda (arg)
                    (if (should-box (unbox arg))
                        `(set! ,(unbox arg) (builtin cons '#f (ref ,(unbox arg))))
                        `(begin)))
                     (improper->proper (args 'raw)))
           ,@(cdr b)))

       ((ref ,identifier) (identifier)
        (if (should-box (identifier 'raw))
            `(builtin cdr (ref ,(identifier)))
            `(ref ,(identifier))))))

    (define frontend
      (concat-passes
       import-and-macroexpand
       track-defines
       normalize-forms
       resolve-names
       check-args
       flag-sets
       box-sets))

    ))
