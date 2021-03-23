#include <runtime/calling.h>
#include <assert.h>
#include <stdio.h>

// $2b$ = +
pscheme_fn($2b$) {
    pscheme_start();
    long acc = 0;
    for (long i = 0; i < pscheme_nargs(); ++i) {
        acc += pscheme_arg(i);
    }
    pscheme_return(acc);
}

// <=
pscheme_fn($3c$$3d$) {
    pscheme_start();
    assert(pscheme_nargs() == 2);
    long a = (long) pscheme_arg(0);
    long b = (long) pscheme_arg(1);
    pscheme_return(a <= b);
}
