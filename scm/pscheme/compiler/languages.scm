(define-library (pscheme compiler languages)
  (import (scheme base)
          (pscheme compiler util)
          (pscheme compiler nanopass))
  (export lscheme
          normal-scheme)
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
        (define-syntax ,identifier ,any)
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
        (builtin ,identifier ,@expression)
        (ffi-symbol ,identifier)
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
         ,literal))))

    ))
