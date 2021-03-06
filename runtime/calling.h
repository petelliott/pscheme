#ifndef PSCHEME_CALLING_H
#define PSCHEME_CALLING_H

#include <stdint.h>
#include <stddef.h>
#include <runtime/object.h>

#define pscheme_start()                                                 \
    pscheme_t  *_pscheme_args;                                          \
    size_t     _pscheme_nargs;                                          \
    asm volatile ("mov %%rbp, %0" : "=r" (_pscheme_nargs));             \
    asm volatile ("mov %%r13, %0" : "=r" (_pscheme_args));              \
    _pscheme_nargs = (((uintptr_t) _pscheme_args) - _pscheme_nargs)/sizeof(pscheme_t) - 2;

#define pscheme_nargs() (_pscheme_nargs)
#define pscheme_arg(n) (_pscheme_args[-n-1])

#define pscheme_return(val)                                        \
    pscheme_t result = val;                                        \
    asm volatile ("mov %0, %%r13" :: "r" (_pscheme_args) : "r13"); \
    return result;

#define pscheme_fn(name)                                                \
    static pscheme_t name(void);                                        \
    __attribute__((aligned(16)))                                        \
    pscheme_t (*_wrapper_##name)(void) = name;                          \
    pscheme_t (**pscm_sym_##name)(void) = &_wrapper_##name;             \
    static pscheme_t name(void)

#endif
