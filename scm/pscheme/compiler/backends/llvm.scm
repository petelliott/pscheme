(define-library (pscheme compiler backends llvm)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 28)
          (pscheme string)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler languages)
          (pscheme compiler file)
          (pscheme compiler nanopass)
          (pscheme compiler backend)
          (only (gauche base) sys-system))
  (export llvm)
  (begin

    (define ll-port (make-parameter #f))
    (define (f str . rest)
      (display (apply format str rest) (ll-port)))

    (define (zero-pad str n)
      (string-append
       (make-string (- n (string-length str)) #\0)
       str))

    (define (string->asm str)
      (string-append
       "\""
       (apply string-append
              (map (lambda (c)
                     (format "\\~a" (zero-pad (number->string (char->integer c) 16) 2)))
                   (string->list str)))
       "\""))

    (define-syntax enum
      (syntax-rules ()
        ((_ name values ...)
         (begin
           (define cnt -1)
           (define values (begin (set! cnt (+ cnt 1)))) ...))))

    (enum tags
          PSCM-T-FIXNUM
          PSCM-T-CONS
          PSCM-T-SINGLETON
          PSCM-T-STRING
          PSCM-T-CHAR
          PSCM-T-CLOSURE
          PSCM-T-SYMBOL
          PSCM-T-FOREIGN)

    (enum singletons
          PSCM-S-NIL
          PSCM-S-F
          PSCM-S-T
          PSCM-S-EOF
          PSCM-S-UNSPECIFIED
          PSCM-S-UNBOUND)

    (define max-fixnum (expt 2 60))
    (define (numeric-representation n)
      (* (modulo n max-fixnum) 16))

    (define (tag-pointer ptr tag)
      (+ ptr tag))

    (define (tag-number num tag)
      (+ (numeric-representation num) tag))

    (define (data-name value)
      (match value
       ((data ,type ,key ,number)
        (format "pscheme_~a_~a" key number))
       ((data ,type ,library-name ,key ,number)
        (format "pscheme_~a_~a_~a" (mangle-library library-name) key number))))

    (define (data-lltype data)
      (if (string? (car data))
          (format "type [~a x i8]" (string-length (car data)))
          (format "type {~a}"
                  (string-join
                   ", "
                   (map (lambda (lit) "i64") data)))))

    (define (data-repr value)
      (cond
       ((integer? value) (tag-number value PSCM-T-FIXNUM))
       ((char? value) (tag-number (char->integer value) PSCM-T-CHAR))
       ((null? value) (tag-number PSCM-S-NIL PSCM-T-SINGLETON))
       ((eq? value #f) (tag-number PSCM-S-F PSCM-T-SINGLETON))
       ((eq? value #t) (tag-number PSCM-S-t PSCM-T-SINGLETON))
       ((is-syntax? 'data value)
        (if (eq? (cadr value) 'none)
            (format "ptrtoint (%~a_typ* @~a to i64)" (data-name value) (data-name value))
            (format "add (i64 ptrtoint (%~a_typ* @~a to i64), i64 ~a)"
                    (data-name value)
                    (data-name value)
                    (case (cadr value)
                      ((string) PSCM-T-STRING)
                      ((symbol) PSCM-T-SYMBOL)
                      ((pair) PSCM-T-CONS)
                      (else (error "can't tag data type" (cadr value)))))))
       (else (error "can't represent value" value))))

    (define-pass llvm-codegen (ir)
      (toplevel-def
       ((lambda ,data-name (,@identifier) ,any ,@instruction) (dname args rest insts)
        (f "define private i64 @~a([0 x i64]* %closure, i64 %nargs~a~a) {\n"
           (data-name (dname 'raw))
           (apply string-append (map (lambda (a) (format ", i64 ~a" a)) (strip-spans (args))))
           (if (rest 'raw) ", ..." ""))
        (insts)
        (f "    ret i64 0\n}\n\n"))
       ((data ,data-name ,@any) (name contents)
        (define c (contents 'raw))
        (define n (data-name (name 'raw)))
        (define lltype (data-lltype (contents 'raw)))
        (f "%~a_typ = ~a\n" n lltype)
        (f "@~a = private global %~a_typ " n n)
        (if (string? (car c))
            (f "c~a" (string->asm (car c)))
            (f "{~a}" (string-join ", " (map (lambda (d) (format "i64 ~a" (data-repr d))) c))))
        (f "\n\n"))
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

      (identifier
       ((local ,number) (n)
        (format "%l~a" (n 'raw)))
       ((arg ,number) (n)
        (format "%a~a" (n 'raw)))
       ((global ,library-name ,symbol) (l s)
        (format "@~a" (mangle (l 'raw) (s 'raw))))
       ((closure ,number) (n)
        (n 'raw))
       ((ffi ,symbol) (sym)
        (format "@~a" (sym 'raw)))
       ((tmp ,number) (n)
        (format "%t~a" (n 'raw)))))

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
