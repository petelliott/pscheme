#ifndef CALLING_H
#define CALLING_H

#include <stdint.h>
#include <stddef.h>

// TODO: move this to object.h
typedef uintptr_t pscheme_t;

#define pscheme_start()                                                 \
    size_t     _pscheme_nargs;                                          \
    asm volatile ("mov %%rbp, %0" : "=r" (_pscheme_nargs));                      \
    _pscheme_nargs = (((uintptr_t) _pscheme_args) - _pscheme_nargs)/sizeof(pscheme_t) - 2;
// TODO: get the closure


#define pscheme_nargs() (_pscheme_nargs)
#define pscheme_arg(n) (_pscheme_args[-n-1])

#define pscheme_return(val)                     \
    asm volatile (                              \
    "mov %%rbp, %%rsp\n\t"                      \
    "pop %%rbp\n\t"                             \
    "pop %%r11\n\t"                             \
    "mov %0, %%rsp\n\t"                         \
    "push %%r11\n\t"                            \
    "ret"                                       \
    :                                           \
    : "r" (_pscheme_args), "a" (val)            \
    :);                                         \
    __builtin_unreachable ();


#define pscheme_fn(name)                                        \
    static void name(pscheme_t *_pscheme_args);                 \
    void (*pscm_sym_##name)(pscheme_t *_pscheme_args) = name;   \
    static void name(pscheme_t *_pscheme_args)

#endif
