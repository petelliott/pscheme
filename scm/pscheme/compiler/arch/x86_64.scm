(define-library (pscheme compiler arch x86_64)
  (import (scheme base)
          (scheme cxr)
          (srfi 28)
          (srfi 1)
          (pscheme compiler util)
          (pscheme compiler arch))
  (export x86_64)
  (begin

    (define word-size 8)

    ;; registers:
    ;; result register:     %rax (caller saved)
    ;; frame pointer:       %rbp (callee saved)
    ;; argument pointer:    %rdi (caller and callee saved)
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
        (format "~a(%rdi)" (- (* (+ (cadr ref) 1) word-size))))
       ((is-syntax? 'global ref)
        (format "~a(%rip)" (mangle (cadr ref) (caddr ref))))
       (else (error "unsupported reference format " ref))))

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
          PSCM-T-CHAR)

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

    (define (add a b)
      ;; TODO: elide mov if one argument is result
      (format "    mov ~a, %rax\n    add ~a, %rax\n"
              (x86-arg a)
              (x86-arg b)))

    (define (sub a b)
      ;; TODO: elide mov if a is result
      (format "    mov ~a, %rax\n    sub ~a, %rax\n"
              (x86-arg a)
              (x86-arg b)))

    (define (mov src dest)
      (format "    movq ~a, ~a\n" (x86-arg src) (x86-arg dest)))

    (define (stack-alloc n)
      (format "    sub $~a, %rsp\n" (* word-size n)))

    (define (stack-free n)
      (format "    add $~a, %rsp\n" (* word-size n)))

    (define (push value)
      (format "    pushq ~a\n" (x86-arg value)))

    (define (pop)
      "    popq %rax\n")

;;; complex syntax operations

    (define (if-prologue key)
      (format "    cmp $~a, %rax\n    je _if_false_~a\n" (tag-number PSCM-S-F PSCM-T-SINGLETON) key))

    (define (if-middle key)
      (format "    jmp _if_end_~a\n_if_false_~a:\n" key key))

    (define (if-end key)
      (format "_if_end_~a:\n" key))

    (define (global-define-slot lib sym)
      (define label (mangle lib sym))
      ;; TODO: replace 0 with undefined constant
      (format "\n    .data\n    .global ~a\n    .align 8\n~a:\t.8byte 0\n" label label))

    (define (load-lambda label)
      (format "    lea ~a(%rip), %rax\n" label))

    ;(define (load-literal literal)
    ;  (format "    mov $~a, %rax\n" literal))

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
      (format "\n    .data\n    .align 16\n~a:\t.8byte ~a, ~a\n" label left right))

    (define (string-literal label value)
      (format "\n    .data\n    .align 16\n~a:\t.asciz ~s\n" label value))

    (define (tag-label label tag)
      (format "(~a + ~a)" label
              (case tag
                ((pair) PSCM-T-CONS)
                ((string) PSCM-T-STRING))))


    (define (load-immediate-literal literal)
      (format "    mov $~a, %rax\n" literal))

    (define (load-data-literal literal)
      (format "    lea ~a(%rip), %rax\n" literal))

;;; closures
    (define (enclose args)
      (format "    mov %rax, %r12\n    push %rdi\n    mov $~a, %rdi\n    call pscheme_allocate_block\n    pop %rdi\n    mov %r12, 0(%rax)\n~a"
              (+ 1 (* word-size (length args)))
              (apply string-append
                     (map (lambda (arg i)
                            (format "    mov ~a, %rcx\n    mov %rcx, ~a(%rax)\n" (x86-arg arg) (* (+ i 1) word-size)))
                          args (iota (length args))))))

;;; pscheme calling convention
    (define (prologue label)
      (format "\n    .text\n~a:\n    push %rbx\n    mov 8(%rdi), %rbx\n    push %rbp\n    mov %rsp, %rbp\n" label))

    (define (accumulate-rest nregular ref)
      (format "    mov $~a, %rsi\n    call pscm_internal_rest\n    mov %rax, ~a\n"
              nregular (x86-arg ref)))

    (define (epilogue)
      ;"    mov %rbp, %rsp\n    pop %rbp\n    pop %r11\n    mov %rdi, %rsp\n    push %r11\n    ret\n")
      "    mov %rbp, %rsp\n    pop %rbp\n    pop %rbx\n    ret\n")

    (define (prepare)
      "    push %rax\n    push %rdi\n")

    (define (pusharg)
      "    push %rax\n")

    (define (call nargs)
      (format "    lea ~a(%rsp), %rdi\n    call *~a(%rsp)\n    mov %rdi, %rsp\n    pop %rdi\n    add $8, %rsp\n"
              (* word-size nargs)
              (* word-size (+ nargs 1))))

    ;; like call but with the double indirection for the closure
    (define (call-closure nargs)
      (format "    lea ~a(%rsp), %rdi\n    mov ~a(%rsp), %rax\n    call *(%rax)\n    mov %rdi, %rsp\n    pop %rdi\n    add $8, %rsp\n"
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

    (define (builtin-eq? nargs)
      (define eqt (genlabel "eqt"))
      (unless (= nargs 2)
        (error "builtin-eq? does not yet support nargs of" nargs))
      (format "    mov $~a, %rax\n    mov (%rsp), %rcx\n    cmp 8(%rsp), %rcx\n    je ~a\n    mov $~a, %rax\n~a:\n"
              (singleton-literal #t) eqt (singleton-literal #f) eqt))

    (define (builtin-typep nargs tag)
      (define label (genlabel "typep"))
      (unless (= nargs 1)
        (error "builtin-typep nargs =" nargs))
      (format "    mov $~a, %rax\n    mov (%rsp), %rcx\n    and $0xf, %rcx\n    cmp $~a, %rcx\n    je ~a\n    mov $~a, %rax\n~a:\n"
              (singleton-literal #t) tag label (singleton-literal #f) label))

    (define (builtin-ptr->ffi nargs)
      (unless (= nargs 1)
        (error "builtin-ptr->ffi nargs =" nargs))
      (format "    mov (%rsp), %rax\n    shr $4, %rax\n    shl $4, %rax"))

    (define (builtin-num->ffi nargs)
      (unless (= nargs 1)
        (error "builtin-num->ffi nargs =" nargs))
      (format "    mov (%rsp), %rax\n    shr $4, %rax"))

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
      `((eq? . ,builtin-eq?)
        (fixnum? . ,(lambda (nargs) (builtin-typep nargs PSCM-T-FIXNUM)))
        (pair? . ,(lambda (nargs) (builtin-typep nargs PSCM-T-CONS)))
        (string? . ,(lambda (nargs) (builtin-typep nargs PSCM-T-STRING)))
        (char? . ,(lambda (nargs) (builtin-typep nargs PSCM-T-CHAR)))
        (fixnum->ffi . ,builtin-num->ffi)
        (string->ffi . ,builtin-ptr->ffi)
        (char->ffi . ,builtin-ptr->ffi)))

    (define (builtin op nargs)
      ((cdr (assoc op builtins)) nargs))

    (define (push-builtin-arg)
      "    push %rax\n")

    (define (pop-builtin-args n)
      (format "    add $~a, %rsp\n" (* n word-size)))

;;; exported architecture

    (define x86_64
      `((add . ,add)
        (sub . ,sub)
        (mov . ,mov)
        (stack-alloc . ,stack-alloc)
        (stack-free . ,stack-free)
        (push . ,push)
        (pop . ,pop)
        (if-prologue . ,if-prologue)
        (if-middle . ,if-middle)
        (if-prologue . ,if-prologue)
        (if-end . ,if-end)
        (global-define-slot . ,global-define-slot)
        (load-lambda . ,load-lambda)
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
        (load-immediate-literal . ,load-immediate-literal)
        (load-data-literal . ,load-data-literal)
        (builtin . ,builtin)
        (push-builtin-arg . ,push-builtin-arg)
        (pop-builtin-args . ,pop-builtin-args)))

    ))
