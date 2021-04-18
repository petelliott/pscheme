
(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme read)
        (scheme write)
        (scheme process-context))

(import (pscheme compiler arch x86_64)
        (pscheme compiler compile)
        (pscheme compiler library))

(add-to-load-path ".")

(compile-file x86_64 (cadr (command-line)) 'program)
