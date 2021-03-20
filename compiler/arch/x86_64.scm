(define-library (pscheme arch x86_64)
  (import (scheme base)
          (srfi-28))
  (export x86_64)
  (begin

    (define word-size 8)

    (define (x86-arg ref)
      (cond
       ((eq? ref 'result) "%rax")
       ((eq? (car ref) 'stack)
        (format "~a(%ebp)" (* (cdr ref) word-size)))
       ((eq? (car ref) 'temp)
          (format "~a(%esp)" (- (* (cdr ref) word-size))))
       ((eq? (car ref) 'arg)
        ;; +2 is for the closure and the number of arguments
        (format "~a(%r12)" (- (* (+ (cdr ref) 2) word-size))))
       ((eq? (car ref) 'global)
        (format "pscm_slot_~a(%rip)" (cadr ref)))
       ((integer? (car ref))
        (format "$~a" (cdr ref)))
       (else (error "unsupported reference format " ref))))

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

    (define (if-prologue key)
      ;; TODO: replace $0 with false value
      (format "    cmp $0, %rax\n    je _if_false_~a" key))

    (define (if-middle key)
      (format "    jmp _if_end_~a\n_if_false_~a:\n" key key))

    (define (if-end key)
      (format "_if_end_~a:\n" key))

    (define (prologue label)
      (format "\n    .text\n    .global ~a\n~a:\n    push %rbp\n    mov %rsp, %rbp\n" label label))

    (define (epilogue)
      "    mov %rbp, %rsp\n    pop %rbp\n    ret\n")

    (define (global-define-slot sym)
      ;; TODO: replace 0 with undefined constant
      (format "    .data\n    .global pscm_slot_~a\npscm_slot_~a:\t.8byte 0\n" sym sym))

    (define (mov src dest)
      (format "    movq ~a, ~a\n" (x86-arg src) (x86-arg dest)))

    (define (stack-alloc n)
      (format "    sub $~a, %rsp\n" (- (* word-size n))))

    (define (stack-free n)
      (format "    sub $~a, %rsp\n" (* word-size n)))

    (define (push value)
      (format "    pushq ~a\n" (x86-arg value)))

    (define (pop)
      "    popq %rax\n")

    (define (load-lambda label)
      (format "    lea ~a(%rip), %rax\n" label))

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
        (prologue . ,prologue)
        (epilogue . ,epilogue)
        (global-define-slot . ,global-define-slot)
        (load-lambda . ,load-lambda)))

    ))
