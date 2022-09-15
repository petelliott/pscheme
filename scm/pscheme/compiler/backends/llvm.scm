(define-library (pscheme compiler backends llvm)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 28)
          (srfi 1)
          (pscheme string)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler languages)
          (pscheme compiler file)
          (pscheme compiler nanopass)
          (pscheme compiler backend)
          (pscheme compiler library)
          (only (gauche base) sys-system))
  (export llvm)
  (begin

    ;; declaration generation

    (define declared (make-parameter '()))
    (define defined (make-parameter '()))
    (define entrypoints (make-parameter '()))

    (define (do-define name)
      (defined (cons name (defined))))

    (define (do-declare name)
      (declared (cons name (declared))))

    (define (do-entrypoint name)
      (entrypoints (cons name (entrypoints))))

    (define-pass llvm-declare-extern-pass (ir)
      (toplevel-def
       ((define ,library-name ,symbol) (lname sym)
        (do-define (list (lname 'raw) (sym 'raw)))))

      (op
       ((import ,library-name) (lname)
        (do-entrypoint (lname 'raw))))

      (identifier
       ((global ,library-name ,symbol) (lname sym)
        (do-declare (list (lname 'raw) (sym 'raw))))))

    (define (llvm-declare-extern ir)
      (parameterize ((declared '())
                     (defined '())
                     (entrypoints '()))
        (llvm-declare-extern-pass ir)
        (for-each (lambda (lib)
                    (f "declare void @pscm_entry_~a()\n" (mangle-library lib)))
                  (entrypoints))
        (f "\n")
        (for-each (lambda (decl)
                    (unless (member decl (defined))
                      (f "@~a = external global i64\n" (apply mangle decl))))
                  (delete-duplicates (declared)))
        (f "\n")))

    ;; codegen

    (define word-size 8)

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
        (format "pscm_~a_~a" key number))
       ((data ,type ,library-name ,key ,number)
        (format "pscm_~a_~a_~a" (mangle-library library-name) key number))))

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
       ((eq? value #t) (tag-number PSCM-S-T PSCM-T-SINGLETON))
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

    (define current-reg (make-parameter #f))
    (define (preop)
      (if (current-reg)
          (f "    ~a = " (current-reg))
          (f "    ")))

    (define last-label (make-parameter #f))

    (define-pass llvm-codegen (ir)
      (toplevel-def
       ((lambda ,data-name (,@identifier) ,any ,@instruction) (dname args rest insts)
        (f "%~a_typ = type i64 (i64*, i64~a~a)\n"
           (data-name (dname 'raw))
           (apply string-append (map (lambda (a) (format ", i64")) (strip-spans (args))))
           (if (rest 'raw) ", ..." ""))
        (f "define private i64 @~a(i64* %closure, i64 %nargs~a~a) {\n"
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
        (f "define void @pscm_entry_~a() {\n" (mangle-library (lib 'raw)))
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
        (format "%t~a" (n 'raw))))

      (instruction
       ((void ,op) (op)
        (parameterize ((current-reg #f))
          (op)))
       ((,identifier ,op) (ident op)
        (parameterize ((current-reg (ident)))
          (op))))

      (op
       ((import ,library-name) (lname)
        (preop)
        (f "call void() @pscm_entry_~a()\n" (mangle-library (lname 'raw))))
       ((if ,identifier ,identifier (,@instruction) ,identifier (,@instruction)) (condition tphi tbranch fphi fbranch)
        (define u (unique))
        (define tlabel #f)
        (define flabel #f)
        (f "    %ifcond_~a = icmp ne i64 ~a, ~a\n"
           u (strip-spans (condition)) (data-repr #f))
        (f "    br i1 %ifcond_~a, label %iftrue_~a, label %iffalse_~a\n" u u u)
        (f "iftrue_~a:\n" u)
        (set! tlabel
              (parameterize ((last-label (format "iftrue_~a" u)))
                (tbranch)
                (last-label)))
        (f "    br label %ifend_~a\n" u)
        (f "iffalse_~a:\n" u)
        (set! flabel
              (parameterize ((last-label (format "iffalse_~a" u)))
                (fbranch)
                (last-label)))
        (f "    br label %ifend_~a\n" u)
        (last-label (format "ifend_~a" u))
        (f "ifend_~a:\n" u)
        (preop)
        (f "phi i64 [ ~a, %~a ], [ ~a, %~a ]\n"
           (strip-spans (tphi)) tlabel (strip-spans (fphi)) flabel))
       ((load-imm ,any) (val)
        (preop)
        (f "add i64 ~a, 0\n" (data-repr (val 'raw))))
       ((load-special ,symbol) (sym)
        (preop)
        (case (sym 'raw)
          ((unspecified)
           (f "add i64 ~a, 0\n" (tag-number PSCM-T-SINGLETON PSCM-S-UNSPECIFIED)))))
       ((return ,identifier) (ident)
        (preop)
        (f "ret i64 ~a\n" (strip-spans (ident))))
       ((closure ,identifier ,@identifier) (fn params)
        (define fun (fn))
        (define args (params))
        (define u (unique))
        (f "    %closure_~a = call i64*(i64) @pscheme_allocate_block(i64 ~a)\n"
           u
           (* word-size (+ 1 (length args))))
        (f "    %closure_~a_fn = getelementptr i64, i64* %closure_~a, i64 0\n" u u)
        (f "    store i64 ~a, i64* %closure_~a_fn\n" fun u)
        (for-each
         (lambda (param n)
           (f "    %closure_~a_~a = getelementptr i64, i64* %closure_~a, i64 0\n" u n u)
           (f "    store i64 ~a, i64* %closure_~a_~a\n" param u n))
         args
         (iota (length args)))
        (f "    %tagged_closure_~a = ptrtoint i64* %closure_~a to i64\n" u u)
        (preop)
        (f "add i64 %tagged_closure_~a, ~a\n" u PSCM-T-CLOSURE))
       ((closure-ref (closure ,number)) (n)
        (define u (unique))
        (f "    %cref_~a = getelementptr i64, i64* %closure, i64 ~a\n" u (+ (n 'raw) 1))
        (preop)
        (f "load i64, i64* %cref_~a\n" u))
       ((global-ref ,identifier) (ident)
        (preop)
        (f "load i64, i64* ~a\n" (strip-spans (ident))))
       ((global-set! ,identifier ,identifier) (name value)
        (preop)
        (f "store i64 ~a, i64* ~a\n" (strip-spans (value)) (strip-spans (name))))
       ((call ,identifier ,@identifier) (fn args)
        (define u (unique))
        (define args2 (strip-spans (args)))
        (f "    %call_~a_a = lshr i64 ~a, 4\n" u (strip-spans (fn)))
        (f "    %call_~a_b = shl i64 %call_~a_a, 4\n" u u)
        (f "    %call_~a_clos = inttoptr i64 %call_~a_b to i64*\n" u u)
        (f "    %call_~a_fnint = load i64, i64* %call_~a_clos\n" u u)
        (f "    %call_~a_fn = inttoptr i64 %call_~a_fnint to i64(i64*, i64, ...)*\n" u u)
        (preop)
        (f "call i64(i64*, i64, ...) %call_~a_fn(i64* %call_~a_clos, i64 ~a~a)\n" u u
           (length args2)
           (apply string-append (map (lambda (a) (format ", i64 ~a" a)) args2))))

       ))



    (define (llvm-compile ir rootname)
      (define llfile (string-append rootname ".ll"))
      (define objfile (string-append rootname ".o"))
      (call-with-output-file llfile
        (lambda (port)
          (parameterize ((ll-port port)
                         (current-unique 0))
            (f "declare i64* @pscheme_allocate_block(i64)\n\n")
            (llvm-declare-extern ir)
            (llvm-codegen ir))))
      (sys-system (format "llc -filetype=obj ~a -o ~a" llfile objfile))
      objfile)

    (define (llvm-link objs outfile)
      (sys-system (format "clang ~a -o ~a"
                          (string-join " " objs)
                          outfile)))


    (define llvm (make-backend 'llvm llvm-compile llvm-link))


    ))
