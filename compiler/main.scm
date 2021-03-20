
(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme read)
        (scheme write))

;; the loads are not neccessary, or available, when compiled
(cond-expand
  (gauche
   (load "./compiler/util.scm")
   (load "./compiler/arch.scm")
   (load "./compiler/arch/x86_64.scm")
   (load "./compiler/frontend.scm")
   (load "./compiler/codegen.scm")))

(import (pscheme arch)
        (pscheme arch x86_64)
        (pscheme frontend)
        (pscheme codegen))

(define (read-all)
  (define obj (read))
  (if (eof-object? obj)
      '()
      (cons obj (read-all))))

(define program (read-all))

(define frontended (map frontend program))

#;(for-each (lambda (stmt) (write stmt) (newline))
          frontended)

(compile-environment 'stdout x86_64
                     (lambda ()
                       (codegen-main-file frontended)))
