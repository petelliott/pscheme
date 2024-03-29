(define-library (scheme file)
  (import (scheme base)
          (pscheme ffi))
  (export call-with-input-file call-with-output-file with-input-from-file
          with-output-to-file open-input-file open-binary-input-file
          open-output-file open-binary-output-file file-exists?)
  (begin

    (define (call-with-input-file string proc)
      (call-with-port (open-input-file string) proc))

    (define (call-with-output-file string proc)
      (call-with-port (open-output-file string) proc))

    (define (with-input-from-file string thunk)
      (call-with-input-file string
        (lambda (port)
          (parameterize ((current-input-port port))
            (thunk)))))

    (define (with-output-to-file string thunk)
      (call-with-output-file string
        (lambda (port)
          (parameterize ((current-output-port port))
            (thunk)))))

    (define (open-input-file string)
      (make-file-port (builtin ffi-call (ffi-symbol fopen) (builtin string->ffi string) (builtin string->ffi "r")) #t #f))

    (define open-binary-input-file open-input-file)

    (define (open-output-file string)
      (make-file-port (builtin ffi-call (ffi-symbol fopen) (builtin string->ffi string) (builtin string->ffi "w+")) #f #t))

    (define open-binary-output-file open-output-file)

    (define (file-exists? filename)
      (= 0 (builtin ffi-call (ffi-symbol access) (builtin string->ffi filename) (builtin fixnum->ffi 0))))

    ))
