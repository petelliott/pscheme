(define-library (pscheme compiler compile)
  ;; TODO: fix compiler to not require this export-before-import shit
  (export compile-project
          objfile-old
          should-fresh-compile
          compile-file
          add-linked-object
          precompile-lib
          linking-context)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 28)
          (srfi 18)
          (srfi 170)
          (pscheme string)
          (pscheme formatter)
          (pscheme compiler frontend)
          (pscheme compiler middleend)
          (pscheme compiler options)
          (pscheme compiler file)
          (pscheme compiler backend))
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

    (define (needs-compile filename)
      (or (option 'fresh)
          (let ((objfile ((backend-objfile-namer (current-backend)) filename)))
            (or (not (file-exists? objfile))
                (let ((fmtime (time->seconds (file-info:mtime (file-info filename #f))))
                      (omtime (time->seconds (file-info:mtime (file-info objfile #f)))))
                  (<= omtime fmtime))))))

    (define should-fresh-compile (make-parameter #f))
    (define compile-tasks (make-parameter #f))

    (define (status-message fmt . args)
      (when (option 'progress)
        (display "\x1b;[2K\r")
        (display (apply format fmt args))
        (flush-output-port)))

    (define (compile-file filename program-or-lib)
      (define objfile ((backend-objfile-namer (current-backend)) filename))
      (define _dummy (status-message "[READ]        ~a" filename))
      (define program (read-file filename))
      (parameterize ((should-fresh-compile (needs-compile filename)))
        (status-message "[MACROEXPAND] ~a\n" filename)
        (let ((me (map import-pass program)))
          (if (should-fresh-compile)
              (compile-tasks
               (cons
                (lambda ()
                  (status-message "[COMPILING]   ~a" filename)
                  (let* ((fir (map frontend me))
                         (mir (middleend fir program-or-lib)))
                    (writeir-for filename mir)
                    ((backend-compile (current-backend)) mir filename objfile))
                  (status-message "[COMPILED]    ~a\n" filename))
                (compile-tasks)))
              (status-message "[CACHED]      ~a\n" filename))))
      (add-linked-object objfile))

    (define (finish-compiles)
      (for-each (lambda (task) (task)) (compile-tasks)))

    (define (precompile-lib backend filename)
      (parameterize ((current-backend backend))
        (compile-file filename 'library)))

    (define (compile-project backend filename outfile linked-libs)
      (parameterize ((current-backend backend)
                     (compile-tasks '()))
        (linking-context outfile
                         (lambda ()
                           (for-each (lambda (lib) (add-linked-object lib)) linked-libs)
                           (compile-file filename 'program)
                           (finish-compiles)
                           ))))

    ))
