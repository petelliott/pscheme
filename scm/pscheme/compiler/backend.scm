(define-library (pscheme compiler backend)
  (import (scheme base))
  (export make-backend
          backend-name
          backend-compile
          backend-link
          backend-objfile-namer
          current-backend)
  (begin

    (define-record-type backend
      (make-backend name compile link objfile-namer)
      backend?
      (name backend-name)
      (compile backend-compile)
      (link backend-link)
      (objfile-namer backend-objfile-namer))

    (define current-backend (make-parameter #f))

    ))
