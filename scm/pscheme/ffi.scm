(define-library (pscheme ffi)
  (import (scheme base))
  (export type->ffi ; exported to to an unhygenic macro system
          ffi->type
          ff->scheme)
  (begin

    ;; TODO: type checks
    (define-syntax type->ffi
      (syntax-rules (int char char*)
        ((_ int arg) (builtin fixnum->ffi arg))
        ((_ char arg) (builtin char->ffi arg))
        ((_ char* arg) (builtin string->ffi arg))))

    (define-syntax ffi->type
      (syntax-rules (int char void)
        ((_ int arg) (builtin ffi->fixnum arg))
        ((_ char arg) (builtin ffi->fixnum arg))
        ((_ void arg) (begin arg (begin)))))

    (define-syntax ff->scheme
      (syntax-rules ()
        ((_ rettype fn (type var) ...)
         (lambda (var ...)
           (begin
             (set! var (type->ffi type var)) ...)
           (ffi->type rettype (builtin ffi-call (ffi-symbol fn) var ...))))))

    ))
