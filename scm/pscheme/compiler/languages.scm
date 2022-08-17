(define-library (pscheme compiler languages)
  (import (scheme base)
          (pscheme compiler util)
          (pscheme compiler nanopass))
  (export lscheme
          normal-scheme
          ref-scheme
          backend-lang)
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
       (proc-toplevel
        (begin ,@proc-toplevel)
        (define ,identifier ,expression)
        (define (,identifier ,@identifier) ,@proc-toplevel)
        (define-syntax ,identifier ,any)
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
         (define-syntax ,identifier ,any))
        (expression
         (if ,expression ,expression)
         (,expression ,@expression)
         ,literal))
       (+
        (expression
         (call ,expression ,@expression)))))

    (define ref-scheme
      (edit-language
       normal-scheme
       (-
        (identifier symbol?)
        (expression
         (lambda (,@identifier) ,@proc-toplevel)
         (ffi-symbol ,symbol)))
       (+
        (identifier
         (ref (stack ,number))
         (ref (arg ,number))
         (ref (global ,library-name ,symbol))
         (ref (closure ,number))
         (ref (ffi ,symbol)))
        (proc-toplevel
         (accumulate-rest ,number ,any)
         (push-locals ,number))
        (expression
         (closure ,expression ,@any)
         (lambda (,@symbol) ,@proc-toplevel)))))

    (define backend-lang
      (edit-language
       ref-scheme
       (-
        (program-toplevel
         (define-library ,library-name ,@library-declaration)
         (import ,@library-name)))
       (+
        (program-toplevel
         (define-library ,library-name ,@program-toplevel)
         (import ,library-name)))))


    ))
