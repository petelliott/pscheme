(define-library (pscheme compiler backends llvm)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme cxr)
          (srfi 28)
          (srfi 1)
          (gauche base)
          (pscheme base)
          (pscheme string)
          (pscheme match)
          (pscheme compiler util)
          (pscheme compiler options)
          (pscheme compiler languages)
          (pscheme compiler file)
          (pscheme compiler syntax)
          (pscheme compiler nanopass)
          (pscheme compiler backend)
          (pscheme compiler library))
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
                  (delete-duplicates (entrypoints)))
        (f "\n")
        (for-each (lambda (decl)
                    (unless (member decl (defined))
                      (f "@~a = external global i64\n" (apply mangle decl))))
                  (delete-duplicates (declared)))
        (f "\n")))

    (define retained-nodes (make-parameter '()))

    (define-pass llvm-local-debuginfo (ssa-ir)
      (toplevel-def
       ((lambda ,data-name (,@identifier) ,any ,@instruction) (dname args rest insts)
        (parameterize ((metascope (reserve-metadata (dname 'raw)))
                       (retained-nodes '()))
          (map (lambda (arg)
                 (dbg-define-local arg (caddr arg) (cadr arg)))
               (args 'raw))
          (when (rest 'raw)
              (dbg-define-local (rest 'raw) (caddr (rest 'raw)) (length (args 'raw))))
          (insts)
          (subprogram-metadata (dname 'raw) (dbg-lambda-name (dname 'raw)))))
       ((entry main ,@instruction) (insts)
        (define metaname "pscheme_main")
        (parameterize ((metascope (reserve-metadata metaname))
                       (retained-nodes '()))
          (insts)
          (subprogram-metadata metaname metaname)))
       ((entry ,library-name ,@instruction) (lib insts)
        (parameterize ((metascope (reserve-metadata (lib 'raw)))
                       (retained-nodes '()))
          (insts)
          (subprogram-metadata (lib 'raw) (string-join "::" (map (lambda (part) (format "~a" part)) (lib 'raw)))))))

      (op
       ((meta-define ,identifier) (i)
        (define meta (caddr (i 'raw)))
        (dbg-define-local (i 'raw) meta #f))))

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
       "\\00\""))

    (define-syntax enum
      (syntax-rules ()
        ((_ name values ...)
         (begin
           (define cnt -1)
           (define values (begin (set! cnt (+ cnt 1)) cnt)) ...))))

    (enum tags
          PSCM-T-FIXNUM
          PSCM-T-CONS
          PSCM-T-SINGLETON
          PSCM-T-STRING
          PSCM-T-CHAR
          PSCM-T-CLOSURE
          PSCM-T-SYMBOL
          PSCM-T-FOREIGN
          PSCM-T-SLOTS)

    (enum singletons
          PSCM-S-NIL
          PSCM-S-F
          PSCM-S-T
          PSCM-S-EOF
          PSCM-S-UNSPECIFIED
          PSCM-S-UNBOUND)

    (define (tag-number num tag)
      (format "or (i64 shl (i64 ~a, i64 4), i64 ~a)" num tag))

    (define (data-name value)
      (match value
        ((data ,type ,key ,symnum)
         (if (symbol? symnum)
             (format "pscm_~a_~a" key (mangle-sym symnum))
             (format "pscm_~a_~a" key symnum)))
        ((data ,type ,key ,sym ,number)
         (format "pscm_~a_~a_~a" key (mangle-sym sym) number))))

    (define (data-linkage value)
      (match value
        ((data ,type ,key ,symnum)
         (if (symbol? symnum)
             "linkonce"
             "private"))
        (else "private")))

    (define (data-lltype data)
      (if (string? (car data))
          (format "type [~a x i8]" (+ 1 (string-length (car data))))
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

    (define adefs (make-parameter '()))
    (define (mark-already-defined obj)
      (adefs (cons obj (adefs))))
    (define (already-defined? obj)
      (member obj (adefs)))

    (define current-reg (make-parameter #f))
    (define (preop)
      (if (current-reg)
          (f "    ~a = " (current-reg))
          (f "    ")))

    (define last-label (make-parameter #f))

    (define (ret-unspec)
      (f "add i64 ~a, 0~a\n" (tag-number PSCM-S-UNSPECIFIED PSCM-T-SINGLETON) (location)))

    (define-pass llvm-codegen (ssa-ir)
      (toplevel-def
       ((lambda ,data-name (,@identifier) ,any ,@instruction) (dname args rest insts)
        (define meta (get-metadata (dname 'raw)))
        (f "%~a_typ = type i64 (i64*, i64~a~a)\n"
           (data-name (dname 'raw))
           (apply string-append (map (lambda (a) (format ", i64")) (strip-spans (args))))
           (if (rest 'raw) ", ..." ""))
        (f "define internal i64 @~a(i64* %closure, i64 %nargs~a~a) !dbg ~a {\n"
           (data-name (dname 'raw))
           (apply string-append (map (lambda (a) (format ", i64 ~a" a)) (strip-spans (args))))
           (if (rest 'raw) ", ..." "")
           meta)
        (parameterize ((metascope meta))
          (map (lambda (arg raw)
                 (dbg-change-value raw (caddr raw) arg))
               (args)
               (args 'raw))
          (when (rest 'raw)
              (dbg-change-value (rest 'raw) (caddr (rest 'raw)) "%restarg")) ;; TODO: idk why i have to use restarg
          (insts))
        (f "}\n\n"))
       ((data ,data-name ,@any) (name contents)
        (define c (contents 'raw))
        (define n (data-name (name 'raw)))
        (define lltype (data-lltype (contents 'raw)))
        (unless (already-defined? n)
          (mark-already-defined n)
          (f "%~a_typ = ~a\n" n lltype)
          (f "@~a = ~a global %~a_typ " n (data-linkage (name 'raw)) n)
          (if (string? (car c))
              (f "c~a" (string->asm (car c)))
              (f "{~a}" (string-join ", " (map (lambda (d) (format "i64 ~a" (data-repr d))) c))))
          (f ", align 16\n")
          (when (eq? (cadr (name 'raw)) 'symbol)
            (f "@~a_tbl = linkonce global i64 ~a, section \"symbols\"\n" n (data-repr (name 'raw))))
          (f "\n")))
       ((define ,library-name ,symbol) (lib sym)
        (f "@~a = global i64 0~a\n\n"
           (mangle (lib 'raw) (sym 'raw)) (global-var-debug (sym 'raw) #f)))
       ((define ,library-name ,symbol ,number) (lib sym num)
        (f "@~a$~a = private global i64 0~a\n\n"
           (mangle (lib 'raw) (sym 'raw)) (num 'raw) (global-var-debug (sym 'raw) (num 'raw))))
       ((entry main ,@instruction) (insts)
        (define meta (get-metadata "pscheme_main"))
        (f "define void @pscheme_main() !dbg ~a {\n" meta)
        (parameterize ((metascope meta))
          (insts))
        (f "    ret void\n}\n\n"))
       ((entry ,library-name ,@instruction) (lib insts)
        (define name (mangle-library (lib 'raw)))
        (define meta (get-metadata (lib 'raw)))
        (f "@pscm_entry_called_~a = private global i1 0\n" name)
        (f "define void @pscm_entry_~a() !dbg ~a {\n"
           name meta)
        (f "    %called = load i1, i1* @pscm_entry_called_~a\n" name)
        (f "    br i1 %called, label %skip, label %do\n")
        (f "skip:\n")
        (f "    ret void\n")
        (f "do:\n")
        (f "    store i1 1, i1* @pscm_entry_called_~a" name)
        (parameterize ((metascope meta))
          (insts))
        (f "    ret void\n}\n\n")))

      (identifier
       ((local ,number ,var-metadata) (n v)
        (format "%l~a" (n 'raw)))
       ((arg rest ,var-metadata) (v)
        "%restarg")
       ((arg ,number ,var-metadata) (n v)
        (format "%a~a" (n 'raw)))
       ((global ,library-name ,symbol) (l s)
        (format "@~a" (mangle (l 'raw) (s 'raw))))
       ((global ,library-name ,symbol ,number) (l s n)
        (format "@~a$~a" (mangle (l 'raw) (s 'raw)) (n 'raw)))
       ((closure ,number ,var-metadata) (n v)
        (n 'raw))
       ((ffi ,symbol) (sym)
        (format "@~a" (sym 'raw)))
       ((tmp ,number) (n)
        (format "%t~a" (n 'raw))))

      (value
       ((unspecified) ()
        (format "~a" (tag-number PSCM-S-UNSPECIFIED PSCM-T-SINGLETON)))
       ((quote ,any) (value)
        (data-repr (value 'raw))))

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
        (f "    %valist_i = bitcast %va_list* %valist to i8*\n")
        (f "    call void @llvm.va_start(i8* %valist_i)\n")
        (f "    %toget = sub i64 %nargs, ~a\n" (n 'raw))
        (f "    %restarg = call i64 @pscheme_internal_rest(i8* %valist_i, i64 %toget)\n")
        (f "    call void @llvm.va_end(i8* %valist_i)\n"))
       ((import ,library-name) (lname)
        (preop)
        (f "call void() @pscm_entry_~a()~a\n" (mangle-library (lname 'raw)) (location)))
       ((meta-set! ,identifier ,value) (l r)
        (dbg-change-value (l 'raw) (caddr (l 'raw)) (strip-spans (r)))
        (preop)
        (ret-unspec))
       ((if ,value ,value (,@instruction) ,value (,@instruction) (,@phi)) (condition tphi tbranch fphi fbranch otherphis)
        (define u (unique))
        (define tlabel #f)
        (define flabel #f)
        (f "    %ifcond_~a = icmp ne i64 ~a, ~a~a\n"
           u (strip-spans (condition)) (data-repr #f) (location))
        (f "    br i1 %ifcond_~a, label %iftrue_~a, label %iffalse_~a~a\n" u u u (location))
        (f "iftrue_~a:\n" u)
        (set! tlabel
              (parameterize ((last-label (format "iftrue_~a" u)))
                (tbranch)
                (last-label)))
        (f "    br label %ifend_~a~a\n" u (location))
        (f "iffalse_~a:\n" u)
        (set! flabel
              (parameterize ((last-label (format "iffalse_~a" u)))
                (fbranch)
                (last-label)))
        (f "    br label %ifend_~a~a\n" u (location))
        (last-label (format "ifend_~a" u))
        (f "ifend_~a:\n" u)
        (for-each (lambda (phi)
                    (match phi
                      ((phi ,name ,new ,tval ,fval)
                       (f "    ~a = phi i64 [ ~a, %~a ], [ ~a, %~a ]~a\n"
                          new tval tlabel fval flabel (location)))))
                  (strip-spans (otherphis)))
        (preop)
        (f "phi i64 [ ~a, %~a ], [ ~a, %~a ]~a\n"
           (strip-spans (tphi)) tlabel (strip-spans (fphi)) flabel (location))
        (for-each (lambda (phi)
                    (match phi
                      ((phi ,name ,new ,tval ,fval)
                       (dbg-change-value name (caddr name) new))))
                  (strip-spans (otherphis))))
       ((builtin ,symbol ,@value) (sym args)
        (apply emit-builtin (sym 'raw) (strip-spans (args))))
       ((return ,value) (val)
        (preop)
        (f "ret i64 ~a~a\n" (strip-spans (val)) (location)))
       ((closure ,value ,@value) (fn params)
        (define fun (strip-spans (fn)))
        (define args (strip-spans (params)))
        (define u (unique))
        (f "    %closure_~a = call i64*(i64) @pscheme_allocate_block(i64 ~a)~a\n"
           u
           (* word-size (+ 1 (length args)))
           (location))
        (f "    %closure_~a_fn = getelementptr i64, i64* %closure_~a, i64 0~a\n" u u (location))
        (f "    store i64 ~a, i64* %closure_~a_fn~a\n" fun u (location))
        (for-each
         (lambda (param n)
           (f "    %closure_~a_~a = getelementptr i64, i64* %closure_~a, i64 ~a~a\n" u n u (+ n 1) (location))
           (f "    store i64 ~a, i64* %closure_~a_~a~a\n" param u n (location)))
         args
         (iota (length args)))
        (f "    %tagged_closure_~a = ptrtoint i64* %closure_~a to i64~a\n" u u (location))
        (preop)
        (f "add i64 %tagged_closure_~a, ~a~a\n" u PSCM-T-CLOSURE (location)))
       ((closure-ref (closure ,number ,var-metadata)) (n v)
        (define u (unique))
        (f "    %cref_~a = getelementptr i64, i64* %closure, i64 ~a~a\n" u (+ (n 'raw) 1) (location))
        (preop)
        (f "load i64, i64* %cref_~a~a\n" u (location)))
       ((global-ref ,identifier) (ident)
        (preop)
        (f "load i64, i64* ~a~a\n" (strip-spans (ident)) (location)))
       ((global-set! ,identifier ,value) (name value)
        (f "    store i64 ~a, i64* ~a~a\n" (strip-spans (value)) (strip-spans (name)) (location))
        (preop)
        (ret-unspec))
       ((call ,bool ,value ,@value) (tail fn args)
        (define u (unique))
        (define args2 (strip-spans (args)))
        (f "    %call_~a_a = lshr i64 ~a, 4~a\n" u (strip-spans (fn)) (location))
        (f "    %call_~a_b = shl i64 %call_~a_a, 4~a\n" u u (location))
        (f "    %call_~a_clos = inttoptr i64 %call_~a_b to i64*~a\n" u u (location))
        (f "    %call_~a_fnint = load i64, i64* %call_~a_clos~a\n" u u (location))
        (f "    %call_~a_fn = inttoptr i64 %call_~a_fnint to i64(i64*, i64, ...)*~a\n" u u (location))
        (preop)
        (f "call i64(i64*, i64, ...) %call_~a_fn(i64* %call_~a_clos, i64 ~a~a)~a\n" u u
           (length args2)
           (apply string-append (map (lambda (a) (format ", i64 ~a" a)) args2))
           (location)))

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
      (f "    %bcmp_~a_a = icmp ~a i64 ~a, ~a~a\n" u type a b (location))
      ;; convert i1 to pscheme boolean without branching
      (f "    %bcmp_~a_b = zext i1 %bcmp_~a_a to i64~a\n" u u (location))
      (f "    %bcmp_~a_c = shl i64 16, %bcmp_~a_b~a\n" u u (location))
      (preop)
      (f "or i64 %bcmp_~a_c, ~a~a\n" u PSCM-T-SINGLETON (location)))

    (define (fixnum-binop op a b)
      (define u (unique))
      (f "    %fbin_~a_a = ashr i64 ~a, 4~a\n" u a (location))
      (f "    %fbin_~a_b = ashr i64 ~a, 4~a\n" u b (location))
      (f "    %fbin_~a_r = ~a i64 %fbin_~a_a, %fbin_~a_b~a\n" u op u u (location))
      (preop)
      (f "shl i64 %fbin_~a_r, 4~a\n" u (location)))

    (define (tag-typep obj tag)
      (define u (unique))
      (f "    %typep_~a_tag = and i64 ~a, 15~a\n" u obj (location))
      (f "    %typep_~a_cmp = icmp eq i64 %typep_~a_tag, ~a~a" u u tag (location))
      ;; convert i1 to pscheme boolean without branching
      (f "    %typep_~a_b0 = zext i1 %typep_~a_cmp to i64~a\n" u u (location))
      (f "    %typep_~a_b1 = shl i64 16, %typep_~a_b0~a\n" u u (location))
      (preop)
      (f "or i64 %typep_~a_b1, ~a~a\n" u PSCM-T-SINGLETON (location)))

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
    (define-builtin (slots? obj) (tag-typep obj PSCM-T-SLOTS))

    (define-builtin (cons a d)
      (define u (unique))
      (f "    %cons_~a_ptr = call i64*() @pscheme_allocate_cell()~a\n" u (location))
      (f "    %cons_~a_car = getelementptr i64, i64* %cons_~a_ptr, i64 0~a\n" u u (location))
      (f "    store i64 ~a, i64* %cons_~a_car~a\n" a u (location))
      (f "    %cons_~a_cdr = getelementptr i64, i64* %cons_~a_ptr, i64 1~a\n" u u (location))
      (f "    store i64 ~a, i64* %cons_~a_cdr~a\n" d u (location))
      (f "    %cons_~a_int = ptrtoint i64* %cons_~a_ptr to i64~a\n" u u (location))
      (preop)
      (f "or i64 %cons_~a_int, ~a~a\n" u PSCM-T-CONS (location)))

    (define (builtin-slot-ref obj slot)
      (define u (unique))
      (f "    %slot_~a_int0 = lshr i64 ~a, 4~a\n" u obj (location))
      (f "    %slot_~a_int = shl i64 %slot_~a_int0, 4~a\n" u u (location))
      (f "    %slot_~a_cptr = inttoptr i64 %slot_~a_int to i64*~a\n" u u (location))
      (f "    %slot_~a_ptr = getelementptr i64, i64* %slot_~a_cptr, i64 ~a~a\n" u u slot (location))
      (preop)
      (f "load i64, i64* %slot_~a_ptr~a\n" u (location)))

    (define-builtin (slot-ref obj slot)
      (define u (unique))
      (f "    %slot_~a_r = lshr i64 ~a, 4~a\n" u slot (location))
      (builtin-slot-ref obj (format "%slot_~a_r" u)))
    (define-builtin (car pair) (builtin-slot-ref pair 0))
    (define-builtin (cdr pair) (builtin-slot-ref pair 1))

    (define (builtin-set-slot! obj slot value)
      (define u (unique))
      (f "    %slot_~a_int0 = lshr i64 ~a, 4~a\n" u obj (location))
      (f "    %slot_~a_int = shl i64 %slot_~a_int0, 4~a\n" u u (location))
      (f "    %slot_~a_cptr = inttoptr i64 %slot_~a_int to i64*~a\n" u u (location))
      (f "    %slot_~a_ptr = getelementptr i64, i64* %slot_~a_cptr, i64 ~a~a\n" u u slot (location))
      (f "    store i64 ~a, i64* %slot_~a_ptr~a\n" value u (location))
      (preop)
      (ret-unspec))

    (define-builtin (set-slot! obj slot value)
      (define u (unique))
      (f "    %slot_~a_r = lshr i64 ~a, 4~a\n" u slot (location))
      (builtin-set-slot! obj (format "%slot_~a_r" u) value))
    (define-builtin (set-car! pair value) (builtin-set-slot! pair 0 value))
    (define-builtin (set-cdr! pair value) (builtin-set-slot! pair 1 value))

    (define (builtin-alloc len tag)
      (define u (unique))
      (f "    %alloc_~a_len = lshr i64 ~a, 4~a\n" u len (location))
      (f "    %alloc_~a_ptr = call i64*(i64) @pscheme_allocate_block(i64 %alloc_~a_len)~a\n" u u (location))
      (f "    %alloc_~a_int = ptrtoint i64* %alloc_~a_ptr to i64~a\n" u u (location))
      (preop)
      (f "or i64 %alloc_~a_int, ~a~a\n" u tag (location)))

    (define-builtin (alloc-string len) (builtin-alloc len PSCM-T-STRING))

    (define (builtin-alloc-slots len tag)
      (define u (unique))
      ;; times 3 for 8 byte slots
      (f "    %alloc_slots_~a_rlen = shl i64 ~a, 3~a\n" u len (location))
      (builtin-alloc (format "%alloc_slots_~a_rlen" u) tag))

    (define-builtin (alloc-slots len) (builtin-alloc-slots len PSCM-T-SLOTS))

    (define-builtin (string-ref str off)
      (define u (unique))
      (f "    %sr_~a_sint0 = lshr i64 ~a, 4~a\n" u str (location))
      (f "    %sr_~a_sint = shl i64 %sr_~a_sint0, 4~a\n" u u (location))
      (f "    %sr_~a_sptr = inttoptr i64 %sr_~a_sint to i8*~a\n" u u (location))
      (f "    %sr_~a_off = ashr i64 ~a, 4~a\n" u off (location))
      (f "    %sr_~a_cptr = getelementptr i8, i8* %sr_~a_sptr, i64 %sr_~a_off~a" u u u (location))
      (f "    %sr_~a_c = load i8, i8* %sr_~a_cptr~a\n" u u (location))
      (f "    %sr_~a_cint0 = zext i8 %sr_~a_c to i64~a\n" u u (location))
      (f "    %sr_~a_cint = shl i64 %sr_~a_cint0, 4~a\n" u u (location))
      (preop)
      (f "or i64 %sr_~a_cint, ~a~a" u PSCM-T-CHAR (location)))

    (define-builtin (string-set! str off ch)
      (define u (unique))
      (f "    %sr_~a_sint0 = lshr i64 ~a, 4~a\n" u str (location))
      (f "    %sr_~a_sint = shl i64 %sr_~a_sint0, 4~a\n" u u (location))
      (f "    %sr_~a_sptr = inttoptr i64 %sr_~a_sint to i8*~a\n" u u (location))
      (f "    %sr_~a_off = ashr i64 ~a, 4~a\n" u off (location))
      (f "    %sr_~a_cptr = getelementptr i8, i8* %sr_~a_sptr, i64 %sr_~a_off~a" u u u (location))
      (f "    %sr_~a_c0 = ashr i64 ~a, 4~a\n" u ch (location))
      (f "    %sr_~a_c = trunc i64 %sr_~a_c0 to i8~a\n" u u (location))
      (f "    store i8 %sr_~a_c, i8* %sr_~a_cptr~a" u u (location))
      (preop)
      (ret-unspec))

    (define-builtin (strcpy dest src length startd starts)
      (define u (unique))
        (f "    %startd_~a = ashr i64 ~a, 4~a\n" u startd (location))
        (f "    %starts_~a = ashr i64 ~a, 4~a\n" u starts (location))
        (f "    %length_~a = ashr i64 ~a, 4~a\n" u length (location))
        (f "    %dest0_~a  = lshr i64 ~a, 4~a\n" u dest (location))
        (f "    %dest1_~a  = shl i64 %dest0_~a, 4~a\n" u u (location))
        (f "    %dest_~a   = add i64 %dest1_~a, %startd_~a~a\n" u u u (location))
        (f "    %src0_~a   = lshr i64 ~a, 4~a\n" u src (location))
        (f "    %src1_~a   = shl i64 %src0_~a, 4~a\n" u u (location))
        (f "    %src_~a    = add i64 %src1_~a, %starts_~a~a\n" u u u (location))
        (f "    %res_~a    = call i64(i64,i64,i64) @memmove(i64 %dest_~a, i64 %src_~a, i64 %length_~a)~a\n" u u u u (location))
        (preop)
        (f "or i64 %res_~a, ~a~a" u PSCM-T-STRING (location)))

    (define (builtin-num->ffi n)
      (preop)
      (f "ashr i64 ~a, 4~a\n" n (location)))

    (define (builtin-ptr->ffi p)
      (define u (unique))
      (f "    %ptr_~a = lshr i64 ~a, 4~a\n" u p (location))
      (preop)
      (f "shl i64 %ptr_~a, 4~a\n" u (location)))

    (define-builtin (fixnum->ffi n) (builtin-num->ffi n))
    (define-builtin (string->ffi n) (builtin-ptr->ffi n))
    (define-builtin (char->ffi n) (builtin-num->ffi n))

    (define (builtin-ffi->num n tag)
      (define u (unique))
      (f "    %num_~a = shl i64 ~a, 4~a\n" u n (location))
      (preop)
      (f "or i64 %num_~a, ~a~a" u tag (location)))

    (define-builtin (ffi->fixnum n) (builtin-ffi->num n PSCM-T-FIXNUM))
    (define-builtin (ffi->char n) (builtin-ffi->num n PSCM-T-CHAR))

    (define-builtin (ffi-call fn . args)
      (preop)
      (f "call i64(...) ~a(~a)~a\n" fn
         (string-join ", " (map (lambda (a) (format "i64 ~a" a)) args))
         (location)))

    (define (retag n tag)
      (define u (unique))
      (f "    %retag_~a_a = lshr i64 ~a, 4~a\n" u n (location))
      (f "    %retag_~a_b = shl i64 %retag_~a_a, 4~a\n" u u (location))
      (preop)
      (f "or i64 %retag_~a_b, ~a~a\n" u tag (location)))

    (define-builtin (integer->char i) (retag i PSCM-T-CHAR))
    (define-builtin (char->integer c) (retag c PSCM-T-FIXNUM))
    (define-builtin (symbol->string s) (retag s PSCM-T-STRING))
    (define-builtin (string->symbol s) (retag s PSCM-T-SYMBOL))

    (define-builtin (eof-object)
      (preop)
      (f "add i64 0, ~a\n" (tag-number PSCM-S-EOF PSCM-T-SINGLETON)))

    ;; debug info
    (define metadata (make-parameter '()))

    (define (register-metadata key md)
      (define n (if (null? (metadata))
                    0
                    (+ 1 (cdar (metadata)))))
      (f "!~a = ~a\n" n md)
      (metadata (cons (cons key n) (metadata)))
      (format "!~a" n))

    (define (reserve-metadata key)
      (define n (if (null? (metadata))
                    0
                    (+ 1 (cdar (metadata)))))
      (metadata (cons (cons key n) (metadata)))
      (format "!~a" n))

    (define (set-metadata key md)
      (define slot (assoc key (metadata)))
      (f "!~a = ~a\n" (cdr slot) md)
      (format "!~a" (cdr slot)))

    (define (get-metadata key)
      (define n (assoc-ref key (metadata)))
      (if n
          (format "!~a" n)
          #f))

    (define (get-all-metadata key)
      (define (inner lst)
        (cond
         ((null? lst) '())
         ((equal? key (caar lst))
          (cons (format "!~a" (cdar lst))
                (inner (cdr lst))))
         (else
          (inner (cdr lst)))))
      (format "!{~a}" (string-join ", " (inner (metadata)))))

    (define (file-metadata ir)
      (define span (first-span ir))
      (when span
        (set-metadata
         'file (format "!DIFile(filename: \"~a\", directory: \"~a\")"
                       (span-file span)
                       (span-file span))) ;; TODO: use the actual directory name
        (set-metadata
         'compunit (format "distinct !DICompileUnit(language: DW_LANG_C99, file: ~a, producer: \"pscheme\", runtimeVersion: 2, emissionKind: ~a, globals: ~a)"
                           (get-metadata 'file)
                           (if (option 'debug #f) "FullDebug" "NoDebug")
                           (get-all-metadata 'global)))))

    (define (subprogram-metadata key name)
      (set-metadata
       key (format "distinct !DISubprogram(name: \"~a\", scope: ~a, file: ~a, unit: ~a, type: !DISubroutineType(types: !{null}), line: ~a, retainedNodes: !{~a})"
                   name
                   (get-metadata 'file)
                   (get-metadata 'file)
                   (get-metadata 'compunit)
                   (if (current-span)
                       (span-sr (current-span))
                       "0")
                   (string-join ", " (retained-nodes)))))

    (define metascope (make-parameter #f))

    (define (location)
      (if (current-span)
          (format ", !dbg !DILocation(line: ~a, column: ~a, scope: ~a)"
                  (span-sr (current-span))
                  (span-sc (current-span))
                  (metascope))
          ""))

    (define (pscheme-builtin-types)
      (register-metadata 'pscheme_t "!DIBasicType(name: \"pscheme_t\", size: 64, align: 64, encoding: DW_ATE_unsigned)")
      (register-metadata 'size_t "!DIBasicType(name: \"size_t\", size: 64, align: 64, encoding: DW_ATE_unsigned)"))

    (define (global-var-debug name number)
      (define n (if number (format "~a#~a" name number) name))
      (define gv (register-metadata #f (format "!DIGlobalVariable(name: \"~a\", scope: ~a, file: ~a, type: ~a, isLocal: false, isDefinition: true)"
                                               n (get-metadata 'file) (get-metadata 'file) (get-metadata 'pscheme_t))))
      (define gve (register-metadata 'global (format "!DIGlobalVariableExpression(var: ~a, expr: !DIExpression())" gv)))
      (format ", !dbg ~a" gve))

    (define (dbg-define-local ident metadata arg)
      (define sym (vm-sym metadata))
      (define m (register-metadata ident (format "!DILocalVariable(name: \"~a\", ~ascope: ~a, file: ~a, line: ~a, type: ~a)"
                                                 (if (syntax-node? sym)
                                                     (format "~a#~a" (syntax-node-sym sym) (syntax-node-instance sym))
                                                     sym)
                                                 (if arg (format "arg: ~a, " (+ 1 arg)) "")
                                                 (metascope) (get-metadata 'file)
                                                 (span-sr (current-span)) (get-metadata 'pscheme_t))))
      (retained-nodes (cons m (retained-nodes))))

    (define (dbg-change-value ident vm value)
      (f "    call void @llvm.dbg.value(metadata i64 ~a, metadata ~a, metadata ~a)~a\n"
         value
         (get-metadata ident)
         (if (and (vm-ever-set? vm) (vm-ever-enclosed? vm))
             ;; this case is a boxed value in the car of a cons cell
             (get-metadata 'box-expression)
            ; "!DIExpression()"
             "!DIExpression()")
         (location)))

    (define (dbg-lambda-name dname)
      (match dname
       ((data ,type ,key ,number)
        (format "#<~a ~a>" key number))
       ((data ,type ,key ,sym)
        (format "~a" sym))
       ((data ,type ,key ,sym ,number)
        (format "~a" sym))))

    (define (llvm-compile ir rootname objfile)
      (define llfile (string-append rootname ".ll"))
      (call-with-output-file llfile
        (lambda (port)
          (parameterize ((ll-port port)
                         (current-unique 0)
                         (metadata '())
                         (adefs '()))
            (register-metadata 'dwarf-version "!{i32 7, !\"Dwarf Version\", i32 5}")
            (register-metadata 'debuginfo-version "!{i32 2, !\"Debug Info Version\", i32 3}")
            (register-metadata 'box-expression "!DIExpression(DW_OP_constu, 1, DW_OP_minus, DW_OP_deref)")
            (f "!llvm.module.flags = !{~a, ~a}\n"
               (get-metadata 'dwarf-version)
               (get-metadata 'debuginfo-version))
            (reserve-metadata 'file)
            (reserve-metadata 'compunit)
            (f "!llvm.dbg.cu = !{~a}\n" (get-metadata 'compunit))
            (pscheme-builtin-types)
            (f "%va_list = type { i32, i32, i8*, i8* }\n")
            (f "declare i64 @pscheme_internal_rest(i8*,i64)\n")
            (f "declare void @llvm.va_start(i8*)\n")
            (f "declare void @llvm.dbg.value(metadata, metadata, metadata)\n")
            (f "declare void @llvm.va_end(i8*)\n")
            (f "declare i64 @memmove(i64, i64, i64)\n")
            (f "declare i64* @pscheme_allocate_cell()\n")
            (f "declare i64* @pscheme_allocate_block(i64)\n\n")
            (llvm-declare-extern ir)
            (llvm-local-debuginfo ir)
            (f "\n")
            (llvm-codegen ir)
            (file-metadata ir))))
      (sys-system (format "llc --relocation-model=pic -filetype=obj ~a -o ~a" llfile objfile))
      objfile)

    (define (llvm-link objs outfile)
      (sys-system (format "clang ~a -o ~a"
                          (string-join " " objs)
                          outfile)))

    (define (llvm-objfile-name filename)
      (string-append filename ".o"))

    (define llvm (make-backend 'llvm llvm-compile llvm-link llvm-objfile-name))


    ))
