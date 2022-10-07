(define-library (pscheme ffi)
  (export ff->scheme)
  (begin

    ;; TODO: type checks
    (define-syntax type->ffi
      (syntax-rules (int char char* bool pscheme_t)
        ((_ int arg) (builtin fixnum->ffi arg))
        ((_ char arg) (builtin char->ffi arg))
        ((_ char* arg) (builtin string->ffi arg))
        ((_ bool arg)
         (if arg
             (builtin fixnum->ffi 1)
             (builtin fixnum->ffi 0)))
        ((_ pscheme_t arg) arg)))

    (define-syntax ffi->type
      (syntax-rules (int char void bool pscheme_t)
        ((_ int arg) (builtin ffi->fixnum arg))
        ((_ char arg) (builtin ffi->fixnum arg))
        ((_ void arg) (begin arg (begin)))
        ((_ bool arg) (if (builtin eq? arg 0) #f #t))
        ((_ pscheme_t arg) arg)))

    (define-syntax ff->scheme
      (syntax-rules ()
        ((_ rettype fn (type var) ...)
         (lambda (var ...)
           (ffi->type rettype (builtin ffi-call (ffi-symbol fn) (type->ffi type var) ...))))))

    ))
