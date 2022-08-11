(define-library (pscheme compiler languages)
  (import (scheme base)
          (pscheme compiler util)
          (pscheme compiler nanopass))
  (export lscheme)
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
       (identifier symbol?)
       (number number?)
       (unquoted-literal unquoted-literal?)
       (any any?)
       ;; non-terminals
       (program-toplevel
        (begin ,@program-toplevel)
        ,library
        ,proc-toplevel
        ,import-declaration)
       (proc-toplevel
        (begin ,@proc-toplevel)
        ,expression
        ,definition)
       (definition
         (define ,identifier ,expression)
         (define (,identifier ,@identifier) ,@command-or-definition))
       (library
           (define-library ,library-name ,@library-declaration))
       (library-name
        (,@library-name-part))
       (library-name-part
        ,identifier
        ,number)
       (library-declaration
        (export ,@identifier)
        ,import-declaration
        (begin ,@program-toplevel))
       (import-declaration
        (import ,@library-name))
       (expression
        ,identifier
        ,literal
        (lambda ,formals ,@proc-toplevel)
        (if ,expression ,expression ,expression)
        (if ,expression ,expression)
        (set! ,identifier ,expression)
        (,expression ,@expression)) ;; procedure call
       (formals
        ,identifier
        (,@identifier))
       (literal
        (quote ,any)
        ,unquoted-literal)))

    ))
