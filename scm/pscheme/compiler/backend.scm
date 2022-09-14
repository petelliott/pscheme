(define-library (pscheme compiler backend)
  (import (scheme base))
  (export make-backend
          backend-name
          backend-compile
          backend-link
          current-backend)
  (begin

    (define-record-type backend
      (make-backend name compile link)
      backend?
      (name backend-name)
      (compile backend-compile)
      (link backend-link))

    (define current-backend (make-parameter #f))

    ))
