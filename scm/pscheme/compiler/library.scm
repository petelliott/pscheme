(define-library (pscheme compiler library)
  (import (scheme base)
          (scheme file)
          (srfi 1)
          (pscheme compiler compile)
          (pscheme compiler arch)
          (pscheme string))
  (export library?
          library-name
          library-imports
          add-library-import!
          add-library-export!
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
      (make-library name imports exports syntax)
      library?
      (name library-name)
      (imports library-imports set-library-imports!)
      (exports library-exports set-library-exports!)
      (syntax library-syntax set-library-syntax!))

    (define (add-library-import! to-lib import-lib)
      (set-library-imports! to-lib (cons import-lib (library-imports to-lib))))

    (define (add-library-export! to-lib export)
      (set-library-exports! to-lib (cons export (library-exports export))))

    (define (add-library-syntax! lib name syntax)
      (set-library-syntax! lib (cons (cons name syntax) (library-syntax lib))))

    (define libraries '())

    (define (new-library name imports exports)
      (define library (make-library name imports exports '()))
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
          (compile-file (current-target) (library-filename name) 'library))
      (add-library-import! (current-library) (lookup-library name)))

    (define (find-library name curr-lib)
      (or (find (lambda (lib) (member name (library-exports lib)))
                (library-imports curr-lib))
          curr-lib))

    (define (lookup-syntax name)
      (define library (find-library name (current-library)))
      (define entry (assoc name (library-syntax library)))
      (if entry
          (cdr entry)
          #f))

    (define (lookup-global name)
      ;; TODO: we should resolve local shadowing before exported globals
      `(global ,(library-name (find-library name (current-library))) ,name))

    ))
