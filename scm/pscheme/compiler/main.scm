(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme read)
        (scheme write)
        (scheme process-context))

(import (pscheme compiler arch x86_64)
        (pscheme compiler compile)
        (pscheme compiler library))

;; TODO: install this or something
(add-to-load-path ".")
(add-to-load-path "/home/peter/code/scheme/pscheme/scm")

(linking-context "a.out"
                 (lambda ()
                   (add-linked-object "/home/peter/code/scheme/pscheme/runtime/runtime.a")
                   (compile-file x86_64 (cadr (command-line)) 'program)))
