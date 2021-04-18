(define-library (pscheme compiler library)
  (import (scheme base)
          (scheme file)
          (pscheme functional)
          (pscheme string))
  (export library?
          library-name
          library-imports
          library-exports
          new-library
          lookup-library
          add-to-load-path
          library-filename
          current-library
          lookup-global)
  (begin

    (define-record-type library
      (make-library name imports exports)
      library?
      (name library-name)
      (imports library-imports set-library-imports!)
      (exports library-exports))

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

    (define (lookup-global name)
      (define library (find (lambda (lib) (member name (library-exports lib)))
                            (library-imports (current-library))))
      ;; TODO: we should resolve local shadowing before exported globals
      `(global ,(library-name (or library (current-library))) ,name))

    ))
