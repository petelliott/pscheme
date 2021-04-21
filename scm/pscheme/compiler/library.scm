(define-library (pscheme compiler library)
  (import (scheme base)
          (scheme file)
          (pscheme functional)
          (pscheme compiler compile)
          (pscheme compiler arch x86_64)
          (pscheme string))
  (export library?
          library-name
          library-imports
          add-library-import!
          library-exports
          new-library
          lookup-library
          add-to-load-path
          library-filename
          current-library
          lookup-global
          compile-and-import)
  (begin

    (define-record-type library
      (make-library name imports exports)
      library?
      (name library-name)
      (imports library-imports set-library-imports!)
      (exports library-exports))

    (define (add-library-import! to-lib import-lib)
      (set-library-imports! to-lib (cons import-lib (library-imports to-lib))))

    (define libraries '())

    (define (new-library name imports exports)
      (define library (make-library name imports exports))
      (set! libraries (cons library libraries))
      library)

    (define (lookup-library name)
      (find (lambda (lib) (equal? name (library-name lib)))
            libraries))

    (define load-paths '())

    (define (add-to-load-path path)
      (set! load-paths (cons path load-paths)))

    (define (library-filename name)
      (define base-name (string-append (string-join "/" (map symbol->string name)) ".scm"))
      (find file-exists?
            (map (lambda (path) (string-append path "/" base-name))
                 load-paths)))

    (define current-library (make-parameter (new-library '(r7rs-user) '() '())))

    (define (compile-and-import name)
      (or (lookup-library name)
          ;; TODO: set arch dynamically
          (compile-file x86_64 (library-filename name) 'library))
      (add-library-import! (current-library) (lookup-library name)))

    (define (lookup-global name)
      (define library (find (lambda (lib) (member name (library-exports lib)))
                            (library-imports (current-library))))
      ;; TODO: we should resolve local shadowing before exported globals
      `(global ,(library-name (or library (current-library))) ,name))

    ))
