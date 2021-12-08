(import (scheme base)
        (scheme process-context)
        (srfi 1)
        (scheme write))

(import (pscheme base)
        (pscheme compiler arch x86_64)
        (pscheme compiler compile)
        (pscheme compiler library)
        (pscheme compiler options))

(define opts (getopt (command-line)
                     '((include #\I #t)
                       (lib #\L #t)
                       (output #\o #t)
                       (arch #\a #t)
                       (ir #\z #f)))) ; TODO: support arch flag

(add-to-load-path ".")
(for-each add-to-load-path
          (map cdr (filter (lambda (m) (eq? (car m) 'include))
                           opts)))

(define libs
  (map cdr (filter (lambda (m) (eq? (car m) 'lib))
                   opts)))

(optionize ((ir (assoc-ref 'ir opts)))
           (compile-project x86_64 (cadr (assoc 'rest opts))
                            (or (assoc-ref 'output opts) "a.out")
                            libs))
