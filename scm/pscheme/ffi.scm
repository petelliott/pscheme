(define-library (pscheme ffi)
  (export ff->scheme)
  (begin

    ;; TODO: type checks
    (define-syntax type->ffi
      (syntax-rules (int char char*)
        ((_ int arg) (builtin fixnum->ffi arg))
        ((_ char arg) (builtin char->ffi arg))
        ((_ char* arg) (builtin string->ffi arg))
        ((_ bool arg)
         (if arg
             (builtin fixnum->ffi 1)
             (builtin fixnum->ffi 0)))))

    (define-syntax ffi->type
      (syntax-rules (int char void)
        ((_ int arg) (builtin ffi->fixnum arg))
        ((_ char arg) (builtin ffi->fixnum arg))
        ((_ void arg) (begin arg (begin)))
        ((_ bool arg) (if (eq? arg 0) #f #t))))

    (define-syntax ff->scheme
      (syntax-rules ()
        ((_ rettype fn (type var) ...)
         (lambda (var ...)
           (begin
             (set! var (type->ffi type var)) ...)
           (ffi->type rettype (builtin ffi-call (ffi-symbol fn) var ...))))))

    ))
