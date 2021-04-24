#include <runtime/calling.h>
#include <runtime/object.h>
#include <assert.h>

// $2b$ = *
pscheme_fn(scheme$$base$$$2a$) {
    pscheme_start();
    long acc = 0;
    for (long i = 0; i < pscheme_nargs(); ++i) {
        acc *= num(pscheme_arg(i));
    }
    pscheme_return(make_pscm_fixnum(acc));
}

// $2b$ = +
pscheme_fn(scheme$$base$$$2b$) {
    pscheme_start();
    long acc = 0;
    for (long i = 0; i < pscheme_nargs(); ++i) {
        acc += num(pscheme_arg(i));
    }
    pscheme_return(make_pscm_fixnum(acc));
}

// $2d$ = -
pscheme_fn(scheme$$base$$$2d$) {
    pscheme_start();
    assert(pscheme_nargs() >= 1);
    if (pscheme_nargs() == 1) {
        pscheme_return(make_pscm_fixnum(-num(pscheme_arg(0))));
    }

    long acc = num(pscheme_arg(0));
    for (long i = 1; i < pscheme_nargs(); ++i) {
        acc -= num(pscheme_arg(i));
    }
    pscheme_return(make_pscm_fixnum(acc));
}

// <=
pscheme_fn(scheme$$base$$$3c$$3d$) {
    pscheme_start();
    assert(pscheme_nargs() == 2);
    pscheme_t a = pscheme_arg(0);
    pscheme_t b = pscheme_arg(1);
    assert(tag(a) == PSCM_T_FIXNUM);
    assert(tag(b) == PSCM_T_FIXNUM);
    pscheme_return(make_pscm_bool(num(a) <= num(b)));
}

pscheme_fn(scheme$$base$$fixnum$3f$) {
    pscheme_start();
    assert(pscheme_nargs() == 1);
    pscheme_return(make_pscm_bool(tag(pscheme_arg(0)) == PSCM_T_FIXNUM));
}
