
(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme write))

;; the loads are not neccessary, or available, when compiled
(cond-expand
  (gauche
   (load "./arch.scm")
   (load "./arch/x86_64.scm")))

(import (pscheme arch)
        (pscheme arch x86_64))

(define (is-syntax? sym form)
  (and (pair? form)
       (eq? (car form) sym)))
