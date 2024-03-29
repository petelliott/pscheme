(define-library (pscheme compiler library)
  (import (scheme base)
          (scheme file)
          (srfi 1)
          (srfi 28)
          (pscheme compiler compile)
          (pscheme compiler syntax)
          (pscheme string))
  (export library?
          library-name
          library-imports
          library-defines
          add-library-import!
          add-library-export!
          add-library-define!
          add-library-syntax!
          library-exports
          new-library
          lookup-library
          add-to-load-path
          library-filename
          current-library
          lookup-syntax
          lookup-global
          compile-and-import)
  (begin

    (define-record-type library
      (make-library name fresh-compile imports exports defines syntax)
      library?
      (name library-name)
      (fresh-compile library-fresh-compile set-library-fresh-compile!)
      (imports library-imports set-library-imports!)
      (exports library-exports set-library-exports!)
      (defines library-defines set-library-defines!)
      (syntax library-syntax set-library-syntax!))

    (define (add-library-import! to-lib import-lib)
      (set-library-imports! to-lib (cons import-lib (library-imports to-lib))))

    (define (add-library-export! to-lib export)
      (set-library-exports! to-lib (cons export (library-exports to-lib))))

    (define (add-library-define! to-lib name sig)
      (set-library-defines! to-lib (cons (cons name sig) (library-defines to-lib))))

    (define (add-library-syntax! lib name syntax)
      (set-library-syntax! lib (cons (cons name syntax) (library-syntax lib))))

    (define libraries '())

    (define (new-library name imports exports)
      (define library (make-library name (should-fresh-compile) imports exports '() '()))
      (set! libraries (cons library libraries))
      library)

    (define (lookup-library name)
      (find (lambda (lib) (equal? name (library-name lib)))
            libraries))

    (define load-paths '())

    (define (add-to-load-path path)
      (set! load-paths (cons path load-paths)))

    (define (library-filename name)
      (define base-name (string-append (string-join "/" (map (lambda (part) (format "~a" part)) name)) ".scm"))
      (find file-exists?
            (map (lambda (path) (string-append path "/" base-name))
                 load-paths)))

    (define current-library (make-parameter (new-library '(r7rs-user) '() '())))

    (define (compile-and-import name)
      (define lib (or (lookup-library name)
                      (begin
                        (compile-file (library-filename name) 'library)
                        (lookup-library name))))
      (should-fresh-compile (or (library-fresh-compile lib)
                                (should-fresh-compile)))
      (set-library-fresh-compile! (current-library) (should-fresh-compile))
      (add-library-import! (current-library) lib))

    (define (find-library name curr-lib)
      (or (and (assoc name (library-defines curr-lib) syntax-equal?) curr-lib)
          (find (lambda (lib) (member name (library-exports lib)))
                (library-imports curr-lib))))

    (define (lookup-syntax name lib)
      (define library (or (find-library name lib)
                          lib))
      (define entry (assoc name (library-syntax library) syntax-equal?))
      (if entry
          (cdr entry)
          #f))

    (define (lookup-global name l)
      ;; TODO: we should resolve local shadowing before exported globals
      (define lib (find-library name l))
      (and lib
           (if (syntax-node? name)
               `(global ,(library-name lib) ,(syntax-node-sym name) ,(syntax-node-instance name))
               `(global ,(library-name lib) ,name))))

    ))
