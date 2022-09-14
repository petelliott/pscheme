(define-library (pscheme compiler compile)
  (import (scheme base)
          (scheme file)
          (scheme write) ;; tmp
          (srfi 28)
          (pscheme string)
          (pscheme formatter)
          (pscheme compiler frontend)
          (pscheme compiler middleend)
          (pscheme compiler options)
          (pscheme compiler file)
          (pscheme compiler backend)
          (only (gauche base) sys-system))
  (export compile-project
          compile-file
          add-linked-object
          linking-context)
  (begin

    (define (read-file filename)
      (call-with-input-file filename
        (lambda (port)
          (span-read-all port filename))))

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
        ((backend-link (current-backend)) (linker-objs (linker-opts)) outfile)))

    (define (writeir ir port)
      (parameterize ((current-output-port port))
        (for-each (lambda (stmt)
                    (sexp-format
                     '((entry . 1)
                       (lambda . 3))
                     stmt)
                    (newline)
                    (newline))
                  ir)))

    (define (writeir-for filename ir)
      (define irfile (string-append filename ".pir"))
      (when (option 'ir)
        (call-with-output-file irfile
          (lambda (port)
            (writeir (strip-spans ir) port)))))

    (define (compile-file filename program-or-lib)
      (define program (read-file filename))
      (define fir (map frontend program))
      (define mir (middleend fir program-or-lib))
      (writeir-for filename mir)
      (add-linked-object ((backend-compile (current-backend)) mir filename)))

    (define (compile-project backend filename outfile linked-libs)
      (parameterize ((current-backend backend))
        (linking-context outfile
                         (lambda ()
                           (for-each (lambda (lib) (add-linked-object lib)) linked-libs)
                           (compile-file filename 'program)))))

    ))
