(import (scheme base)
        (scheme process-context)
        (srfi 1)
        (scheme write))

(import (pscheme base)
        (pscheme getopt)
        (pscheme compiler backends llvm)
        (pscheme compiler compile)
        (pscheme compiler library)
        (pscheme compiler options))

(define opts (getopt (command-line)
                     '((include #\I #t)
                       (lib #\L #t)
                       (output #\o #t)
                       (debug #\g #f)
                       (ir #\z #f)
                       (progress #\p #f)
                       (fresh #f #f))))

(add-to-load-path ".")
(for-each add-to-load-path
          (map cdr (filter (lambda (m) (eq? (car m) 'include))
                           opts)))

(define libs
  (map cdr (filter (lambda (m) (eq? (car m) 'lib))
                   opts)))

(optionize ((ir (assoc-ref 'ir opts))
            (debug  (assoc-ref 'debug opts))
            (progress (assoc-ref 'progress opts))
            (fresh (assoc-ref 'fresh opts)))
           (compile-project llvm (cadr (assoc 'rest opts))
                            (or (assoc-ref 'output opts) "a.out")
                            libs))
