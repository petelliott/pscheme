(define-library (pscheme compiler backends llvm)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 28)
          (pscheme string)
          (pscheme compiler util)
          (pscheme compiler languages)
          (pscheme compiler nanopass)
          (pscheme compiler backend)
          (only (gauche base) sys-system))
  (export llvm)
  (begin

    (define ll-port (make-parameter #f))
    (define (f str . rest)
      (display (apply format str rest) (ll-port)))


    (define-pass llvm-codegen (ir)
      (toplevel-def
       ((lambda ,data-name (,@identifier) ,any ,@instruction) (dname args rest insts)
        (f "define private i64 @~a(...) {\n" (dname))
        (insts)
        (f "    ret i64 0\n}\n\n"))
       ((data ,data-name ,@any) (name contents)
        (define c (contents 'raw))
        (f "@~a = private global i64 0\n\n" (name)))
       ((define ,library-name ,symbol) (lib sym)
        (f "@~a = global i64 0\n\n" (mangle (lib 'raw) (sym 'raw))))
       ((entry main ,@instruction) (insts)
        (f "define i32 @main() {\n")
        (insts)
        (f "    ret i32 0\n}\n\n"))
       ((entry ,library-name ,@instruction) (lib insts)
        (f "define void @pscheme_entry_~a() {\n" (mangle-library (lib 'raw)))
        (insts)
        (f "    ret void\n}\n\n")))

      (data-name
       ((data ,symbol ,symbol ,number) (type group num)
        (format "pscheme_~a_~a" (group 'raw) (num 'raw)))
       ((data ,symbol ,library-name ,symbol ,number) (type lib group num)
        (format "pscheme_~a_~a_~a" (mangle-library (lib 'raw)) (group 'raw) (num 'raw)))))



    (define (llvm-compile ir rootname)
      (define llfile (string-append rootname ".ll"))
      (define objfile (string-append rootname ".o"))
      (call-with-output-file llfile
        (lambda (port)
          (parameterize ((ll-port port))
            (llvm-codegen ir))))
      (sys-system (format "llc -filetype=obj ~a -o ~a" llfile objfile))
      objfile)

    (define (llvm-link objs outfile)
      (sys-system (format "clang ~a -o ~a"
                          (string-join " " objs)
                          outfile)))


    (define llvm (make-backend 'llvm llvm-compile llvm-link))


    ))
