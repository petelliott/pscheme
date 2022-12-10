
    .text
    .globl pscheme_internal_apply
    .type pscheme_internal_apply, @function
    /* pscheme_t pscheme_internal_apply(pscheme_t closure, size_t nargs, pscheme_t args) */
pscheme_internal_apply:
    .cfi_startproc
    push %rbp
    mov %rsp, %rbp

    mov %rdi, %rax
    shr $4, %rax
    shl $4, %rax
    mov %rax, %rdi
    mov (%rax), %rax
    push %rax      /* fn */
    mov %rdx, %r11 /* args */
    /* make sure stack is 16 byte aligned. ugh */
    sub $8, %rsp

    /* allocate space for the non-register args */
    mov %rsi, %r10
    sub $4, %r10
    cmp $0, %r10
    jle .Lskipstack

    /* make sure stack is 16 byte aligned. ugh */
    mov %r10, %rax
    and $1, %rax
    cmp $0, %rax
    je .Lskipstackpad
    sub $8, %rsp
.Lskipstackpad:

    shl $3, %r10
    sub %r10, %rsp
.Lskipstack:

    /* register arguments */
    /* arg 1 */
    cmp $2, %r11 /* 2 == '() */
    je .Lcall
    shr $4, %r11
    shl $4, %r11
    mov (%r11), %rdx
    mov 8(%r11), %r11
    /* arg 2 */
    cmp $2, %r11 /* 2 == '() */
    je .Lcall
    shr $4, %r11
    shl $4, %r11
    mov (%r11), %rcx
    mov 8(%r11), %r11
    /* arg 3 */
    cmp $2, %r11 /* 2 == '() */
    je .Lcall
    shr $4, %r11
    shl $4, %r11
    mov (%r11), %r8
    mov 8(%r11), %r11
    /* arg 4 */
    cmp $2, %r11 /* 2 == '() */
    je .Lcall
    shr $4, %r11
    shl $4, %r11
    mov (%r11), %r9
    mov 8(%r11), %r11

    /* stack arguments */
    mov $0, %r10
.Lloop:
    cmp $2, %r11
    je .Lcall
    shr $4, %r11
    shl $4, %r11
    mov (%r11), %rax
    mov %rax, (%rsp, %r10, 8)
    mov 8(%r11), %r11
    add $1, %r10
    jmp .Lloop

.Lcall:
    mov -8(%rbp), %r10
    xor %rax, %rax
    call *%r10
    mov %rbp, %rsp
    pop %rbp
    ret
    .cfi_endproc

    .section    .note.GNU-stack,"",@progbits
