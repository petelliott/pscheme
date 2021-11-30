(define-library (pscheme compiler arch x86_64)
  (import (scheme base)
          (scheme cxr)
          (srfi 1)
          (srfi 28)
          (pscheme compiler util)
          (pscheme compiler arch))
  (export x86_64)
  (begin

    (define word-size 8)

    ;; registers:
    ;; result register:     %rax (caller saved)
    ;; frame pointer:       %rbp (callee saved)
    ;; argument pointer:    %r13 (caller saved in pscheme, callee saved in C for ease of ffi)
    ;; closure registers:   %rbx (callee saved)

    ;; temprorary registers: %rcx, %r12

    (define (x86-arg ref)
      (cond
       ((eq? ref 'result) "%rax")
       ((is-syntax? 'stack ref)
        (format "~a(%rbp)" (- (* (+ (cadr ref) 1) word-size))))
       ((is-syntax? 'closure ref)
        (format "~a(%rbx)" (* (+ (cadr ref) 1) word-size)))
       ((is-syntax? 'arg ref)
        (format "~a(%r13)" (- (* (+ (cadr ref) 1) word-size))))
       ((is-syntax? 'global ref)
        (format "~a(%rip)" (mangle (cadr ref) (caddr ref))))
       ((is-syntax? 'immediate ref)
        (format "$~a" (cadr ref)))
       ((is-syntax? 'stack ref)
        (format "~a(%rsp)" (* (cadr ref) word-size)))
       ((eq? ref 'unspecified)
        (format "$~a" (tag-number PSCM-S-UNSPECIFIED PSCM-T-SINGLETON)))
       ((string? ref) ref)
       (else (error "unsupported reference format " ref))))

    (define (x86-data ref)
      (cadr ref))

    (define (x86-arg-lea-safe ref)
      (if (is-syntax? 'data ref)
          (x86-arg 'result)
          (x86-arg ref)))

    (define (ensure-lea-safe ref)
      (if (is-syntax? 'data ref)
          (mov ref 'result)
          ""))

;;; tagged pointer representation
    (define max-fixnum (expt 2 60))
    (define (numeric-representation n)
      (* (modulo n max-fixnum) 16))

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

    (define (tag-pointer ptr tag)
      (+ ptr tag))

    (define (tag-number num tag)
      (+ (numeric-representation num) tag))

;;; general purpose operations:

    (define (mem-ref? r)
      (not (or (eq? 'result r)
               (is-syntax? 'immediate r)
               (eq? 'unspecified r)
               (string? r))))

    (define (mov src dest)
      (cond
       ((equal? src dest) "") ; elide mov %rax, %rax
       ((and (is-syntax? 'data src) (mem-ref? dest))
        (format "    leaq ~a(%rip), %rcx\n    movq %rcx, ~a\n" (cadr src) (x86-arg dest)))
       ((is-syntax? 'data src)
        (format "    leaq ~a(%rip), ~a\n" (cadr src) (x86-arg dest)))
       ((and (mem-ref? src) (mem-ref? dest)) ; you call yourself a CISC architecture?
        (format "    movq ~a, %rcx\n    mov %rcx, ~a\n" (x86-arg src) (x86-arg dest)))
       (else
        (format "    movq ~a, ~a\n" (x86-arg src) (x86-arg dest)))))

    (define (stack-alloc n)
      (format "    sub $~a, %rsp\n" (* word-size n)))

;;; complex syntax operations

    (define (if-prologue value key)
      (format "~a~a    cmpq $~a, ~a\n    je _if_false_~a\n"
              (ensure-lea-safe value)
              (if (is-syntax? 'immediate value) (mov value 'result) "")
              (tag-number PSCM-S-F PSCM-T-SINGLETON)
              (x86-arg-lea-safe (if (is-syntax? 'immediate value) 'result value))
              key))

    (define (if-middle key)
      (format "    jmp _if_end_~a\n_if_false_~a:\n" key key))

    (define (if-end key)
      (format "_if_end_~a:\n" key))

    (define (global-define-slot lib sym)
      (define label (mangle lib sym))
      ;; TODO: replace 0 with undefined constant
      (format "\n    .data\n    .global ~a\n    .align 8\n~a:\t.8byte 0\n" label label))

;;; literal representations
    (define (fixnum-literal value)
      (format "~a" (tag-number value PSCM-T-FIXNUM)))

    (define (char-literal value)
      (format "~a" (tag-number (char->integer value) PSCM-T-CHAR)))

    (define (singleton-literal value)
      (format "~a"
              (tag-number (case value
                              ((()) PSCM-S-NIL)
                              ((#f) PSCM-S-F)
                              ((#t) PSCM-S-T)) PSCM-T-SINGLETON)))

    (define (cons-literal label left right)
      (format "\n    .data\n    .align 16\n~a:\t.8byte ~a, ~a\n" label (x86-data left) (x86-data right)))

    (define (string-literal label value)
      (format "\n    .data\n    .align 16\n~a:\t.asciz ~s\n" label value))

    (define (tag-label label tag)
      (format "(~a + ~a)" label
              (case tag
                ((pair) PSCM-T-CONS)
                ((string) PSCM-T-STRING))))

;;; closures
    (define (enclose l args)
      (format "~a    mov $~a, %rdi\n    call pscheme_allocate_block\n    mov %r12, 0(%rax)\n~a    or $~a, %rax\n"
              (mov l "%r12")
              (+ 1 (* word-size (length args)))
              (apply string-append
                     (map (lambda (arg i)
                            (format "    mov ~a, %rcx\n    mov %rcx, ~a(%rax)\n" (x86-arg arg) (* (+ i 1) word-size)))
                          args (iota (length args))))
              PSCM-T-CLOSURE))

;;; pscheme calling convention
    (define (prologue label)
      (format "\n    .text\n~a:\n    push %rbx\n    mov 8(%r13), %rbx\n    shr $4, %rbx\n    shl $4, %rbx\n    push %rbp\n    mov %rsp, %rbp\n"
              label))

    (define (accumulate-rest nregular ref)
      (format "    mov $~a, %rdi\n    call pscm_internal_rest\n    mov %rax, ~a\n"
              nregular (x86-arg ref)))

    (define (epilogue)
      "    mov %rbp, %rsp\n    pop %rbp\n    pop %rbx\n    ret\n")

    (define (prepare fn)
      (format "~a    push ~a\n    push %r13\n"
              (ensure-lea-safe fn)
              (x86-arg-lea-safe fn)))

    (define (pusharg value)
      (format "~a    push ~a\n"
              (ensure-lea-safe value)
              (x86-arg-lea-safe value)))

    (define (call nargs)
      (format "    lea ~a(%rsp), %r13\n    call *~a(%rsp)\n    mov %r13, %rsp\n    pop %r13\n    add $8, %rsp\n"
              (* word-size nargs)
              (* word-size (+ nargs 1))))

    ;; like call but with the double indirection for the closure
    (define (call-closure nargs)
      (format "    lea ~a(%rsp), %r13\n    mov ~a(%rsp), %rax\n    shr $4, %rax\n    shl $4, %rax\n    call *(%rax)\n    mov %r13, %rsp\n    pop %r13\n    add $8, %rsp\n"
              (* word-size nargs)
              (* word-size (+ nargs 1))))

;;; C calling convention

    (define (c-prologue label)
      (format "\n    .text\n    .global ~a\n~a:\n    push %rbp\n    mov %rsp, %rbp\n" label label))

    (define (c-epilogue)
      "    xor %rax, %rax\n    mov %rbp, %rsp\n    pop %rbp\n    ret\n")

    (define (c-call label)
      (format "    call ~a\n" label))

;;; builtins

    (define (assert-nargs args op target)
      (unless (op (length args) target)
        (error "builtin: wrong number of args" args)))

    (define (builtin-cmp args inst)
      (define cmp-label (genlabel "cmp"))
      (assert-nargs args = 2)
      (format "~a~a    mov $~a, %rax\n    cmpq %r8, %rcx\n    ~a ~a\n    mov $~a, %rax\n~a:\n"
              (mov (cadr args) "%r8")
              (mov (car args) "%rcx")
              (singleton-literal #t)
              inst
              cmp-label (singleton-literal #f) cmp-label))

    (define (builtin-typep args tag)
      (define label (genlabel "typep"))
      (assert-nargs args = 1)
      (format "~a    mov $~a, %rax\n    and $0xf, %rcx\n    cmp $~a, %rcx\n    je ~a\n    mov $~a, %rax\n~a:\n"
              (mov (car args) "%rcx")
              (singleton-literal #t) tag label (singleton-literal #f) label))

    (define (builtin-fixnum-binop args inst)
      (assert-nargs args = 2)
      (format "~a~a    shr $4, %rcx\n    shr $4, %rax\n    ~a %rcx, %rax\n    shl $4, %rax\n    or $~a, %rax\n"
              (mov (cadr args) "%rcx")
              (mov (car args) 'result)
              inst PSCM-T-FIXNUM))

    (define (builtin-ptr->ffi args)
      (assert-nargs nargs = 1)
      (format "~a    shr $4, %rax\n    shl $4, %rax"
              (mov (car args) "%rax")))

    (define (builtin-num->ffi args)
      (assert-nargs nargs = 1)
      (format "~a    shr $4, %rax"
              (mov (car args) "%rax")))


    #;(define (builtin-ffi-call nargs)
      (define regs '("%rdi" "%rsi" "%rdx" "%rcx" "%r8" "%r9"))
      (define (ffi-regarg i)
        (when (>= i 6)
          (error "can't pass nth arg in register:" i))
        (format "    mov ~a(%rsp), ~a\n" (- (* word-size (- nargs i 1))) (list-ref regs i)))
      (define (ffi-pushargs)
        (reduce string-append "" (map ffi-regarg (iota nargs))))
      (format "    push %rdi\n~a\n    "
              (ffi-pushargs)))

    (define builtins
      `((eq? . ,(lambda (args) (builtin-cmp args "je")))
        (fixnum< . ,(lambda (args) (builtin-cmp args "jl")))
        (fixnum<= . ,(lambda (args) (builtin-cmp args "jle")))
        (fixnum+ . ,(lambda (args) (builtin-fixnum-binop args "add")))
        (fixnum* . ,(lambda (args) (builtin-fixnum-binop args "imul")))
        (fixnum- . ,(lambda (args) (builtin-fixnum-binop args "sub")))
        (fixnum/ . ,(lambda (args) (builtin-fixnum-binop args "idiv")))
        (fixnum? . ,(lambda (args) (builtin-typep args PSCM-T-FIXNUM)))
        (pair? . ,(lambda (args) (builtin-typep args PSCM-T-CONS)))
        (string? . ,(lambda (args) (builtin-typep args PSCM-T-STRING)))
        (char? . ,(lambda (args) (builtin-typep args PSCM-T-CHAR)))
        (procedure? . ,(lambda (args) (builtin-typep args PSCM-T-CLOSURE)))
        (fixnum->ffi . ,builtin-num->ffi)
        (string->ffi . ,builtin-ptr->ffi)
        (char->ffi . ,builtin-ptr->ffi)))

    (define (builtin op nargs)
      ((cdr (assoc op builtins)) nargs))

    (define (push-builtin-arg ref)
      (format "    push ~a\n" (x86-arg ref)))

    (define (pop-builtin-args n)
      (format "    add $~a, %rsp\n" (* n word-size)))

;;; exported architecture

    (define x86_64
      `((mov . ,mov)
        (stack-alloc . ,stack-alloc)
        (if-prologue . ,if-prologue)
        (if-middle . ,if-middle)
        (if-prologue . ,if-prologue)
        (if-end . ,if-end)
        (global-define-slot . ,global-define-slot)
        (enclose . ,enclose)
        (prologue . ,prologue)
        (accumulate-rest . ,accumulate-rest)
        (epilogue . ,epilogue)
        (prepare . ,prepare)
        (pusharg . ,pusharg)
        (call . ,call)
        (call-closure . ,call-closure)
        (c-prologue . ,c-prologue)
        (c-epilogue . ,c-epilogue)
        (c-call . ,c-call)
        (fixnum-literal . ,fixnum-literal)
        (char-literal . ,char-literal)
        (singleton-literal . ,singleton-literal)
        (cons-literal . ,cons-literal)
        (string-literal . ,string-literal)
        (tag-label . ,tag-label)
        (builtin . ,builtin)
        (push-builtin-arg . ,push-builtin-arg)
        (pop-builtin-args . ,pop-builtin-args)))

    ))
