(define-library (pscheme compiler languages)
  (import (scheme base)
          (pscheme compiler util)
          (pscheme compiler file)
          (pscheme compiler nanopass))
  (export lscheme
          normal-scheme
          make-var-metadata
          var-metadata?
          vm-ever-set?
          vm-set-ever-set!
          vm-ever-enclosed?
          vm-set-ever-enclosed!
          vm-sym
          vm-span
          make-box
          box?
          unbox
          ref-scheme
          ir
          ssa-ir)
  (begin

    (define (unquoted-literal? form)
      (or (number? form)
          (string? form)
          (vector? form)
          (boolean? form)
          (char? form)))

    (define lscheme
      (language
       ;; terminals
       (symbol symbol?)
       (identifier symbol?)
       (number number?)
       (literal unquoted-literal?)
       (any any?)
       ;; non-terminals
       (program-toplevel
        (begin ,@program-toplevel)
        (define-library ,library-name ,@library-declaration)
        (import ,@library-name)
        ,proc-toplevel)
       (syntax-transformer
        (syntax-rules (,@symbol) ,@any))
       (proc-toplevel
        (begin ,@proc-toplevel)
        (define ,identifier ,expression)
        (define (,identifier ,@identifier) ,@proc-toplevel)
        (define-syntax ,identifier ,syntax-transformer)
        ,expression)
       (library-name
        (,@library-name-part))
       (library-name-part
        ,symbol
        ,number)
       (library-declaration
        (export ,@identifier)
        (import ,@library-name)
        (begin ,@program-toplevel))
       (expression
        (begin ,@expression)
        (lambda (,@identifier) ,@proc-toplevel)
        (if ,expression ,expression ,expression)
        (if ,expression ,expression)
        (set! ,identifier ,expression)
        (quote ,any)
        (builtin ,symbol ,@expression)
        (ffi-symbol ,symbol)
        (,expression ,@expression) ;; procedure call
        ,identifier
        ,literal)))

    (define normal-scheme
      (edit-language
       lscheme
       (-
        (literal unquoted-literal?)
        (proc-toplevel
         (define (,identifier ,@identifier) ,@proc-toplevel)
         (define-syntax ,identifier ,syntax-transformer))
        (expression
         (lambda (,@identifier) ,@proc-toplevel)
         (if ,expression ,expression)
         (,expression ,@expression)
         ,literal))
       (+
        (lambda-name
         (anon)
         ,symbol)
        (expression
         (lambda ,lambda-name (,@identifier) ,@proc-toplevel)
         (call ,expression ,@expression)))))

    (define-record-type var-metadata
      (make-var-metadata sym-span ever-set ever-enclosed)
      var-metadata?
      (sym-span vm-sym-span)
      (ever-set vm-ever-set? vm-set-ever-set!)
      (ever-enclosed vm-ever-enclosed? vm-set-ever-enclosed!))

    (define (vm-sym vm)
      (unspan1 (vm-sym-span vm)))

    (define (vm-span vm)
      (and (span? (vm-sym-span vm))
clo           (vm-sym-span vm)))

    ;; TODO: this is a really ugly hack
    (define-record-type box
      (make-box obj)
      box?
      (obj unbox))

    (define ref-scheme
      (edit-language
       normal-scheme
       (-
        (identifier symbol?)
        (expression
         (lambda ,lambda-name (,@identifier) ,@proc-toplevel)
         (ffi-symbol ,symbol)))
       (+
        (box box?)
        (var-metadata var-metadata?)
        (identifier
         (local ,number ,var-metadata)
         (arg rest ,var-metadata)
         (arg ,number ,var-metadata)
         (global ,library-name ,symbol)
         (closure ,number ,var-metadata)
         (ffi ,symbol))
        (proc-toplevel
         (accumulate-rest ,number))
        (expression
         (ref ,identifier)
         (lambda ,lambda-name (,@box) ,@proc-toplevel)
         (closure ,expression ,@identifier)))))

    (define ir
      (language
       (symbol symbol?)
       (number number?)
       (any any?)
       (string string?)
       (program
        (,@toplevel-def))
       (library-name
        (,@library-name-part))
       (library-name-part
        ,symbol
        ,number)
       (data-name
        (data ,symbol ,symbol ,number)
        (data ,symbol ,symbol ,symbol)
        (data ,symbol ,symbol ,symbol ,number))
       (identifier
        (local ,number)
        (arg rest)
        (arg ,number)
        (global ,library-name ,symbol)
        (closure ,number)
        (ffi ,symbol)
        (tmp ,number))
       (immediate
        (quote ,any)
        ,identifier)
       (toplevel-def
        (lambda ,data-name (,@identifier) ,any ,@instruction)
        (data ,data-name ,@any)
        (define ,library-name ,symbol)
        (entry main ,@instruction)
        (entry ,library-name ,@instruction))
       (instruction
        (void ,op)
        (,identifier ,op))
       (op
        (nop)
        (accumulate-rest ,number)
        (import ,library-name)
        (if ,identifier ,identifier (,@instruction) ,identifier (,@instruction))
        (load-imm ,any)
        (load-special ,symbol)
        (builtin ,symbol ,@identifier)
        (set! ,identifier ,identifier)
        (return ,identifier)
        (closure ,identifier ,@identifier)
        (closure-ref (closure ,number))
        (global-ref ,identifier)
        (global-set! ,identifier ,identifier)
        (call ,identifier ,@identifier))))

    (define ssa-ir
      (edit-language
       ir
       (-
        (op
         (set! ,identifier ,identifier)
         (if ,identifier ,identifier (,@instruction) ,identifier (,@instruction))))
       (+
        (phi
         (phi ,identifier ,identifier ,identifier))
        (op
         (if ,identifier ,identifier (,@instruction) ,identifier (,@instruction) (,@phi))))))

    ))
