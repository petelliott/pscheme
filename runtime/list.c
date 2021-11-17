#include <runtime/calling.h>
#include <runtime/object.h>
#include <assert.h>

pscheme_fn(scheme$$base$$cons) {
    pscheme_start();
    assert(pscheme_nargs() == 2);

    pscheme_return(pscheme_cons(pscheme_arg(0), pscheme_arg(1)));
}

pscheme_fn(scheme$$base$$car) {
    pscheme_start();
    assert(pscheme_nargs() == 1);
    pscheme_return(pscheme_car(pscheme_arg(0)));
}

pscheme_fn(scheme$$base$$cdr) {
    pscheme_start();
    assert(pscheme_nargs() == 1);
    pscheme_return(pscheme_cdr(pscheme_arg(0)));
}
