(define-library (pscheme compiler backends llvm)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme cxr)
          (srfi 28)
          (srfi 1)
          (pscheme base)
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
    (define ffi (make-parameter '()))

    (define (do-define name)
      (defined (cons name (defined))))

    (define (do-declare name)
      (declared (cons name (declared))))

    (define (do-entrypoint name)
      (entrypoints (cons name (entrypoints))))

    (define (do-ffi name)
      (ffi (cons name (ffi))))

    (define-pass llvm-declare-extern-pass (ssa-ir)
      (toplevel-def
       ((define ,library-name ,symbol) (lname sym)
        (do-define (list (lname 'raw) (sym 'raw)))))

      (op
       ((import ,library-name) (lname)
        (do-entrypoint (lname 'raw))))

      (identifier
       ((global ,library-name ,symbol) (lname sym)
        (do-declare (list (lname 'raw) (sym 'raw))))
       ((ffi ,symbol) (sym)
        (do-ffi (sym 'raw)))))

    (define (llvm-declare-extern ir)
      (parameterize ((declared '())
                     (defined '())
                     (entrypoints '()))
        (llvm-declare-extern-pass ir)
        (for-each (lambda (name)
                    (f "declare i64 @~a(...)\n" name))
                  (delete-duplicates (ffi)))
        (f "\n")
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

    (define (ret-unspec)
      (f "add i64 ~a, 0\n" (tag-number PSCM-S-UNSPECIFIED PSCM-T-SINGLETON)))

    (define-pass llvm-codegen (ssa-ir)
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
        (f ", align 16\n\n"))
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
       ((arg rest) ()
        "%restarg")
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
       ((accumulate-rest ,number) (n)
        (f "    %valist = alloca %va_list\n")
        (f "    call void @llvm.va_start(%va_list* %valist)\n")
        (f "    %toget = sub i64 %nargs, ~a" (n 'raw))
        (f "    %restarg = call i64 @pscheme_internal_rest(%va_list* %valist, i64 %toget)\n")
        (f "    call void @llvm.va_end(%va_list* %valist)\n"))
       ((import ,library-name) (lname)
        (preop)
        (f "call void() @pscm_entry_~a()\n" (mangle-library (lname 'raw))))
       ((if ,identifier ,identifier (,@instruction) ,identifier (,@instruction) (,@phi)) (condition tphi tbranch fphi fbranch otherphis)
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
        (for-each (lambda (phi)
                    (f "    ~a = phi i64 [ ~a, %~a ], [ ~a, %~a ]\n"
                       (cadr phi) (caddr phi) tlabel (cadddr phi) flabel))
                  (strip-spans (otherphis)))
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
           (ret-unspec))))
       ((builtin ,symbol ,@identifier) (sym args)
        (apply emit-builtin (sym 'raw) (strip-spans (args))))
       ((return ,identifier) (ident)
        (preop)
        (f "ret i64 ~a\n" (strip-spans (ident))))
       ((closure ,identifier ,@identifier) (fn params)
        (define fun (strip-spans (fn)))
        (define args (strip-spans (params)))
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

    (define builtins '())
    (define-syntax define-builtin
      (syntax-rules ()
        ((_ (name . args) body ...)
         (set! builtins
               (cons (cons 'name (lambda args body ...))
                     builtins)))))
    (define (emit-builtin sym . args)
      (define fun (assoc-ref sym builtins))
      (if fun
          (begin
            (f "    ; begin builtin ~a\n" sym)
            (apply fun args)
            (f "    ; end builtin ~a\n" sym))
          (error "no builtin" sym)))

    (define (icmp type a b)
      (define u (unique))
      (f "    %bcmp_~a_a = icmp ~a i64 ~a, ~a\n" u type a b)
      ;; convert i1 to pscheme boolean without branching
      (f "    %bcmp_~a_b = zext i1 %bcmp_~a_a to i64\n" u u)
      (f "    %bcmp_~a_c = shl i64 16, %bcmp_~a_b\n" u u)
      (preop)
      (f "or i64 %bcmp_~a_c, ~a\n" u PSCM-T-SINGLETON))

    (define (fixnum-binop op a b)
      (define u (unique))
      (f "    %fbin_~a_a = ashr i64 ~a, 4\n" u a)
      (f "    %fbin_~a_b = ashr i64 ~a, 4\n" u b)
      (f "    %fbin_~a_r = ~a i64 %fbin_~a_a, %fbin_~a_b\n" u op u u)
      (preop)
      (f "shl i64 %fbin_~a_r, 4\n" u))

    (define (tag-typep obj tag)
      (define u (unique))
      (f "    %typep_~a_tag = and i64 ~a, 15\n" u obj)
      (f "    %typep_~a_cmp = icmp eq i64 %typep_~a_tag, ~a" u u tag)
      ;; convert i1 to pscheme boolean without branching
      (f "    %typep_~a_b0 = zext i1 %typep_~a_cmp to i64\n" u u)
      (f "    %typep_~a_b1 = shl i64 16, %typep_~a_b0\n" u u)
      (preop)
      (f "or i64 %typep_~a_b1, ~a\n" u PSCM-T-SINGLETON))

    (define-builtin (eq? a b) (icmp 'eq a b))
    (define-builtin (fixnum< a b) (icmp 'slt a b))
    (define-builtin (fixnum<= a b) (icmp 'sle a b))
    (define-builtin (fixnum+ a b) (fixnum-binop 'add a b))
    (define-builtin (fixnum* a b) (fixnum-binop 'mul a b))
    (define-builtin (fixnum- a b) (fixnum-binop 'sub a b))
    (define-builtin (fixnum/ a b) (fixnum-binop 'sdiv a b))
    (define-builtin (fixnum-remainder a b) (fixnum-binop 'srem a b))
    (define-builtin (fixnum? obj) (tag-typep obj PSCM-T-FIXNUM))
    (define-builtin (pair? obj) (tag-typep obj PSCM-T-CONS))
    (define-builtin (string? obj) (tag-typep obj PSCM-T-STRING))
    (define-builtin (symbol? obj) (tag-typep obj PSCM-T-SYMBOL))
    (define-builtin (char? obj) (tag-typep obj PSCM-T-CHAR))
    (define-builtin (procedure? obj) (tag-typep obj PSCM-T-CLOSURE))

    (define-builtin (cons a d)
      (define u (unique))
      (f "    %cons_~a_ptr = call i64*() @pscheme_allocate_cell()\n" u)
      (f "    %cons_~a_car = getelementptr i64, i64* %cons_~a_ptr, i64 0\n" u u)
      (f "    store i64 ~a, i64* %cons_~a_car\n" a u)
      (f "    %cons_~a_cdr = getelementptr i64, i64* %cons_~a_ptr, i64 1\n" u u)
      (f "    store i64 ~a, i64* %cons_~a_cdr\n" d u)
      (f "    %cons_~a_int = ptrtoint i64* %cons_~a_ptr to i64\n" u u)
      (preop)
      (f "or i64 %cons_~a_int, ~a\n" u PSCM-T-CONS))

    (define (builtin-car/cdr off pair)
      (define u (unique))
      (f "    %cadr_~a_int0 = lshr i64 ~a, 4\n" u pair)
      (f "    %cadr_~a_int = shl i64 %cadr_~a_int0, 4\n" u u)
      (f "    %cadr_~a_cptr = inttoptr i64 %cadr_~a_int to i64*\n" u u)
      (f "    %cadr_~a_ptr = getelementptr i64, i64* %cadr_~a_cptr, i64 ~a\n" u u off)
      (preop)
      (f "load i64, i64* %cadr_~a_ptr\n" u))
    (define-builtin (car pair) (builtin-car/cdr 0 pair))
    (define-builtin (cdr pair) (builtin-car/cdr 1 pair))

    (define (builtin-set-car/cdr! off pair value)
      (define u (unique))
      (f "    %cadr_~a_int0 = lshr i64 ~a, 4\n" u pair)
      (f "    %cadr_~a_int = shl i64 %cadr_~a_int0, 4\n" u u)
      (f "    %cadr_~a_cptr = inttoptr i64 %cadr_~a_int to i64*\n" u u)
      (f "    %cadr_~a_ptr = getelementptr i64, i64* %cadr_~a_cptr, i64 ~a\n" u u off)
      (f "    store i64 ~a, i64* %cadr_~a_ptr\n" value u)
      (preop)
      (ret-unspec))
    (define-builtin (set-car! pair value) (builtin-set-car/cdr! 0 pair value))
    (define-builtin (set-cdr! pair value) (builtin-set-car/cdr! 1 pair value))

    (define (builtin-alloc len tag)
      (define u (unique))
      (f "    %alloc_~a_ptr = call i64*(i64) @pscheme_allocate_block(i64 ~a)\n" u len)
      (f "    %alloc_~a_int = ptrtoint i64* %alloc_~a_ptr to i64\n" u u)
      (preop)
      (f "or i64 %alloc_~a_int, ~a\n" u tag))

    (define-builtin (alloc-string len) (builtin-alloc len PSCM-T-STRING))

    (define-builtin (string-ref str off)
      (define u (unique))
      (f "    %sr_~a_sint0 = lshr i64 ~a, 4\n" u str)
      (f "    %sr_~a_sint = shl i64 %sr_~a_sint0, 4\n" u u)
      (f "    %sr_~a_sptr = inttoptr i64 %sr_~a_sint to i8*\n" u u)
      (f "    %sr_~a_off = ashr i64 ~a, 4\n" u off)
      (f "    %sr_~a_cptr = getelementptr i8, i8* %sr_~a_sptr, i64 %sr_~a_off" u u u)
      (f "    %sr_~a_c = load i8, i8* %sr_~a_cptr\n" u u)
      (f "    %sr_~a_cint0 = zext i8 %sr_~a_c to i64\n" u u)
      (f "    %sr_~a_cint = shl i64 %sr_~a_cint0, 4\n" u u)
      (preop)
      (f "or i64 %sr_~a_cint, ~a" u PSCM-T-CHAR))

    (define-builtin (string-set! str off ch)
      (define u (unique))
      (f "    %sr_~a_sint0 = lshr i64 ~a, 4\n" u str)
      (f "    %sr_~a_sint = shl i64 %sr_~a_sint0, 4\n" u u)
      (f "    %sr_~a_sptr = inttoptr i64 %sr_~a_sint to i8*\n" u u)
      (f "    %sr_~a_off = ashr i64 ~a, 4\n" u off)
      (f "    %sr_~a_cptr = getelementptr i8, i8* %sr_~a_sptr, i64 %sr_~a_off" u u u)
      (f "    %sr_~a_c0 = ashr i64 ~a, 4\n" u ch)
      (f "    %sr_~a_c = trunc i64 %sr_~a_c0 to i8\n" u u)
      (f "    store i8 %sr_~a_c, i8* %sr_~a_cptr" u u)
      (preop)
      (ret-unspec))

    (define-builtin (strcpy dest src length startd starts)
      (define u (unique))
        (f "    %startd_~a = ashr i64 ~a, 4\n" u startd)
        (f "    %starts_~a = ashr i64 ~a, 4\n" u starts)
        (f "    %length_~a = ashr i64 ~a, 4\n" u length)
        (f "    %dest0_~a  = lshr i64 ~a, 4\n" u dest)
        (f "    %dest1_~a  = shl i64 %dest0_~a, 4\n" u u)
        (f "    %dest_~a   = add i64 %dest1_~a, %startd_~a\n" u u u)
        (f "    %src0_~a   = lshr i64 ~a, 4\n" u src)
        (f "    %src1_~a   = shl i64 %src0_~a, 4\n" u u)
        (f "    %src_~a    = add i64 %src1_~a, %starts_~a\n" u u u)
        (f "    %res_~a    = call i64(i64,i64,i64) @memmove(i64 %dest_~a, i64 %src_~a, i64 %length_~a)\n" u u u u)
        (preop)
        (f "or i64 %res_~a, ~a" u PSCM-T-STRING))

    (define (builtin-num->ffi n)
      (preop)
      (f "ashr i64 ~a, 4\n"n ))

    (define (builtin-ptr->ffi p)
      (define u (unique))
      (f "    %ptr_~a = lshr i64 ~a, 4\n" u p)
      (preop)
      (f "shl i64 %ptr_~a, 4\n" u))

    (define-builtin (fixnum->ffi n) (builtin-num->ffi n))
    (define-builtin (string->ffi n) (builtin-ptr->ffi n))
    (define-builtin (char->ffi n) (builtin-num->ffi n))

    (define (builtin-ffi->num n tag)
      (define u (unique))
      (f "    %num_~a = shl i64 ~a, 4\n" u n)
      (preop)
      (f "or i64 %num_~a, ~a" u tag))

    (define-builtin (ffi->fixnum n) (builtin-ffi->num n PSCM-T-FIXNUM))
    (define-builtin (ffi->char n) (builtin-ffi->num n PSCM-T-CHAR))

    (define-builtin (ffi-call fn . args)
      (preop)
      (f "call i64(...) ~a(~a)\n" fn
         (string-join ", " (map (lambda (a) (format "i64 ~a" a)) args))))

    (define (retag n tag)
      (define u (unique))
      (f "    %retag_~a = lshr i64 ~a, 4\n" u n)
      (preop)
      (f "or i64 %retag_~a, ~a\n" u tag))

    (define-builtin (integer->char i) (retag i PSCM-T-CHAR))
    (define-builtin (char->integer i) (retag i PSCM-T-FIXNUM))

    (define (llvm-compile ir rootname)
      (define llfile (string-append rootname ".ll"))
      (define objfile (string-append rootname ".o"))
      (call-with-output-file llfile
        (lambda (port)
          (parameterize ((ll-port port)
                         (current-unique 0))
            (f "%va_list = type { i32, i32, i8*, i8* }\n")
            (f "declare i64 @pscheme_internal_rest(%va_list*,i64)\n")
            (f "declare void @llvm.va_start(%va_list*)\n")
            (f "declare void @llvm.va_end(%va_list*)\n")
            (f "declare i64 @memmove(i64, i64, i64)\n")
            (f "declare i64* @pscheme_allocate_cell()\n")
            (f "declare i64* @pscheme_allocate_block(i64)\n\n")
            (llvm-declare-extern ir)
            (llvm-codegen ir))))
      (sys-system (format "clang -g -c ~a -o ~a" llfile objfile))
      objfile)

    (define (llvm-link objs outfile)
      (sys-system (format "clang -g ~a -o ~a"
                          (string-join " " objs)
                          outfile)))

    (define llvm (make-backend 'llvm llvm-compile llvm-link))


    ))
