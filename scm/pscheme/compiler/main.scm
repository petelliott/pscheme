(import (scheme base)
        (scheme process-context)
        (srfi 1))

(import (pscheme base)
        (pscheme compiler arch x86_64)
        (pscheme compiler compile)
        (pscheme compiler library))

(define opts (getopt (command-line)
                     '((include #\I #t)
                       (lib #\L #t)
                       (output #\o #t)
                       (arch #\a #t)))) ; TODO: support arch flag

(add-to-load-path ".")
(for-each add-to-load-path
          (map cdr (filter (lambda (m) (eq? (car m) 'include))
                           opts)))

(define libs
  (map cdr (filter (lambda (m) (eq? (car m) 'lib))
                   opts)))

(compile-project x86_64 (cadr (assoc 'rest opts))
                 (cdr (or (assoc 'output opts) '(output . "a.out")))
                 libs)
