(define-library (pscheme compiler compile)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (srfi-28)
          (pscheme string)
          (pscheme compiler arch)
          (pscheme compiler arch x86_64)
          (pscheme compiler frontend)
          (pscheme compiler codegen)
          (pscheme compiler options)
          (pscheme compiler writeir)
          (only (gauche base) sys-system))
  (export compile-project
          compile-file
          add-linked-object
          linking-context)
  (begin

    (define (read-all . args)
      (define obj (apply read args))
      (if (eof-object? obj)
          '()
          (cons obj (apply read-all args))))

    (define (read-file filename)
      (call-with-input-file filename
        (lambda (port)
          (read-all port))))

    (define-record-type linker-options
      (make-linker-options objects)
      linker-options?
      (objects linker-objs set-linker-objs!))

    (define linker-opts (make-parameter (make-linker-options '())))

    (define (add-linked-object name)
      (set-linker-objs! (linker-opts) (cons name (linker-objs (linker-opts)))))

    (define (linking-context outfile proc)
      (parameterize ((linker-opts (make-linker-options '())))
        (proc)
        (sys-system (format "gcc -g -Og ~a -o ~a"
                            (string-join " " (linker-objs (linker-opts)))
                            outfile))))

    (define (writeir-for filename ir)
      (define irfile (string-append filename ".pir"))
      (when (option 'ir)
        (call-with-output-file irfile
          (lambda (port)
            (writeir ir port)))))

    (define (compile-file target filename program-or-lib)
      (define asmfile (string-append filename ".s"))
      (define objfile (string-append filename ".o"))
      (compile-environment asmfile target
                           (lambda ()
                             (define ir (map frontend (read-file filename)))
                             (writeir-for filename ir)
                             (case program-or-lib
                               ((program) (codegen-main-file ir))
                               ((library) (codegen-library-file ir))
                               (else (error "invalid argument to compile-file: " program-or-lib)))))
      (sys-system (format "gcc -g -Og -c ~a -o ~a" asmfile objfile))
      (add-linked-object objfile))

    (define (compile-project target filename outfile linked-libs)
      (linking-context outfile
                       (lambda ()
                         (for-each (lambda (lib) (add-linked-object lib)) linked-libs)
                         (compile-file target filename 'program))))

    ))
