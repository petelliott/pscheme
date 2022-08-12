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
        ,expression)
       (library-name
        (,@library-name-part))
       (library-name-part
        ,identifier
        ,number)
       (library-declaration
        (export ,@identifier)
        (import ,@library-name)
        (begin ,@program-toplevel))
       (expression
        (lambda (,@identifier) ,@proc-toplevel)
        (if ,expression ,expression ,expression)
        (if ,expression ,expression)
        (set! ,identifier ,expression)
        (quote ,any)
        (,expression ,@expression) ;; procedure call
        ,identifier
        ,literal)))

    ))
