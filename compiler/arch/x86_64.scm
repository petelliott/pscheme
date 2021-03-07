(define-library (pscheme arch x86_64)
  (import (scheme base)
          (srfi-28))
  (export x86_64)
  (begin

    (define word-size 8)

    (define (x86-arg ref)
      (cond
       ((eq? ref 'result) "%rax")
       ((eq? (car ref) 'b)
        (format "~a(%ebp)" (* (cdr ref) word-size)))
       ((eq? (car ref) 's)
        (format "~a(%esp)" (- (* (cdr ref) word-size))))
       ((eq? (car ref) 'a)
        (format "~a(%r12)" (- (* (cdr ref) word-size))))
       ((eq? (car ref) 'global)
        (format "~a" (cdr ref)))
       ((integer? (car ref))
        (format "$~a" (cdr ref)))))

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

    (define x86_64 `((add . ,add)
                     (sub . ,sub)
                     (if-prologue . ,if-prologue)
                     (if-middle . ,if-middle)
                     (if-prologue . ,if-prologue)
                     (if-end . ,if-end)))

    ))
