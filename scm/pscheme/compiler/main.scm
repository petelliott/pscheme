(import (scheme base)
        (scheme file)
        (scheme process-context)
        (srfi 1)
        (srfi 170)
        (scheme write)
        (scheme read))

(import (pscheme base)
        (pscheme getopt)
        (pscheme path)
        (pscheme compiler backends llvm)
        (pscheme compiler compile)
        (pscheme compiler library)
        (pscheme compiler options))

;; load pre-define command-line from .pscheme-config
(define executable (read-symlink "/proc/self/exe"))
(define config-file (string-append (dirname executable) "/." (basename executable) "-pscmconfig"))
(define hard-config
  (if (file-exists? config-file)
      (call-with-input-file config-file read)
      '()))

(define opts (getopt (cons (car (command-line)) (append hard-config (cdr (command-line))))
                     '((include #\I #t)
                       (lib #\L #t)
                       (output #\o #t)
                       (debug #\g #f)
                       (ir #\z #f)
                       (progress #\p #f)
                       (release #f #f)
                       (fresh #f #f)
                       (library #f #f))))

(add-to-load-path ".")
(for-each add-to-load-path
          (map cdr (filter (lambda (m) (eq? (car m) 'include))
                           opts)))

(define libs
  (map cdr (filter (lambda (m) (eq? (car m) 'lib))
                   opts)))

(optionize ((ir (assoc-ref 'ir opts))
            (debug (or (assoc-ref 'release opts) (assoc-ref 'debug opts)))
            (progress (assoc-ref 'progress opts))
            (fresh (assoc-ref 'fresh opts)))
           (if (assoc-ref 'library opts)
               (precompile-lib llvm (cadr (assoc 'rest opts)))
               (compile-project llvm (cadr (assoc 'rest opts))
                                (or (assoc-ref 'output opts) "a.out")
                                libs)))
