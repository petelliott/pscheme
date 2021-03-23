(define-library (pscheme arch x86_64)
  (import (scheme base)
          (srfi-28)
          (pscheme util))
  (export x86_64)
  (begin

    (define word-size 8)

    ;; registers:
    ;; result register:     %rax (caller saved)
    ;; frame pointer:       %rbp (callee saved)
    ;; argument pointer:    %rdi (caller and callee saved)
    ;; temporary registers: %r11 (caller saved)

    (define (x86-arg ref)
      (cond
       ((eq? ref 'result) "%rax")
       ((is-syntax? 'stack ref)
        (format "~a(%ebp)" (* (cdr ref) word-size)))
       #;((is-syntax? 'temp ref)
          (format "~a(%esp)" (- (* (cdr ref) word-size))))
       ((is-syntax? 'arg ref)
        (format "~a(%rdi)" (- (* (+ (cadr ref) 1) word-size))))
       ((is-syntax? 'global ref)
        (format "~a(%rip)" (mangle (cadr ref))))
       ((integer? ref)
        (format "$~a" ref))
       (else (error "unsupported reference format " ref))))

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
      ;; TODO: replace $0 with false value
      (format "    cmp $0, %rax\n    je _if_false_~a\n" key))

    (define (if-middle key)
      (format "    jmp _if_end_~a\n_if_false_~a:\n" key key))

    (define (if-end key)
      (format "_if_end_~a:\n" key))

    (define (global-define-slot sym)
      (define label (mangle sym))
      ;; TODO: replace 0 with undefined constant
      (format "    .data\n    .global ~a\n~a:\t.8byte 0\n" label label))

    (define (load-lambda label)
      (format "    lea ~a(%rip), %rax\n" label))


;;; pscheme calling convention
    (define (prologue label)
      (format "\n    .text\n~a:\n    push %rbp\n    mov %rsp, %rbp\n" label))

    (define (epilogue)
      ;"    mov %rbp, %rsp\n    pop %rbp\n    pop %r11\n    mov %rdi, %rsp\n    push %r11\n    ret\n")
      "    mov %rbp, %rsp\n    pop %rbp\n    ret\n")

    (define (prepare)
      "    push %rax\n    push %rdi\n")

    (define (pusharg)
      "    push %rax\n")

    (define (call nargs)
      (format "    lea ~a(%rsp), %rdi\n    call *~a(%rsp)\n    mov %rdi, %rsp\n    pop %rdi\n    add $8, %rsp\n"
              (* word-size nargs)
              (* word-size (+ nargs 1))))

;;; C calling convention

    (define (c-prologue label)
      (format "\n    .text\n    .global ~a\n~a:\n    push %rbp\n    mov %rsp, %rbp\n" label label))

    (define (c-epilogue)
      "    mov %rbp, %rsp\n    pop %rbp\n    ret\n")

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
        (prologue . ,prologue)
        (epilogue . ,epilogue)
        (prepare . ,prepare)
        (pusharg . ,pusharg)
        (call . ,call)
        (c-prologue . ,c-prologue)
        (c-epilogue . ,c-epilogue)))

    ))
