#include <runtime/calling.h>
#include <stdio.h>

// $2b$ = '+
pscheme_fn($2b$) {
    pscheme_start();
    long acc = 0;
    for (long i = 0; i < pscheme_nargs(); ++i) {
        acc += pscheme_arg(i);
    }
    pscheme_return(acc);
}
