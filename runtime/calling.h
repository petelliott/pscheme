#ifndef PSCHEME_CALLING_H
#define PSCHEME_CALLING_H

#include <stdint.h>
#include <stddef.h>

// TODO: move this to object.h
typedef uintptr_t pscheme_t;

#define pscheme_start()                                                 \
    size_t     _pscheme_nargs;                                          \
    asm volatile ("mov %%rbp, %0" : "=r" (_pscheme_nargs));             \
    _pscheme_nargs = (((uintptr_t) _pscheme_args) - _pscheme_nargs)/sizeof(pscheme_t) - 2;
// TODO: get the closure


#define pscheme_nargs() (_pscheme_nargs)
#define pscheme_arg(n) (_pscheme_args[-n-1])

#define pscheme_return(val)                                        \
    asm volatile ("mov %0, %%rdi" :: "r" (_pscheme_args) : "rdi"); \
    return val;


#define pscheme_fn(name)                                        \
    static pscheme_t name(pscheme_t *_pscheme_args);            \
    pscheme_t (*pscm_sym_##name)(pscheme_t *_pscheme_args) = name;   \
    static pscheme_t name(pscheme_t *_pscheme_args)

#endif
