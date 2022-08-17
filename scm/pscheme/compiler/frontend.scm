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
          `(define-library ,library-name ,@library-declaration)))
       ((import ,@library-name) (names)
        (do-import (names))
        `(import ,@(names))))

      (library-declaration
       ((export ,@identifier) (names)
        (for-each (lambda (name)
                    (add-library-export! (current-library)))
                  names)
        '(export ,@(names)))
       ((import ,@identifier) (names)
        (do-import (names))
        `(export ,@(names))))

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

    (define frontend
      (concat-passes
       (list
        import-and-macroexpand)))

    ))
