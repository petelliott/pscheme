#include <runtime/calling.h>
#include <runtime/object.h>
#include <assert.h>
#include <stdio.h>

// $2b$ = +
pscheme_fn(scheme$$base$$$2b$) {
    pscheme_start();
    long acc = 0;
    for (long i = 0; i < pscheme_nargs(); ++i) {
        acc += num(pscheme_arg(i));
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

pscheme_fn(scheme$$base$$newline) {
    pscheme_start();
    assert(pscheme_nargs() == 0);
    printf("\n");
    pscheme_return(PSCM_UNSPECIFIED);
}

// TODO: write this in scheme.
static void pscheme_write(pscheme_t obj) {
    if (tag(obj) == PSCM_T_FIXNUM) {
        printf("%li", num(obj));
    } else if (tag(obj) == PSCM_T_CONS) {
        printf("(");
        pscheme_write(pscheme_car(obj));
        printf(" . ");
        pscheme_write(pscheme_cdr(obj));
        printf(")");
    } else if (tag(obj) == PSCM_T_SINGLETON) {
        if (unum(obj) == PSCM_S_NIL) {
            printf("()");
        } else if (unum(obj) == PSCM_S_F) {
            printf("#f");
        } else if (unum(obj) == PSCM_S_T) {
            printf("#t");
        } else if (unum(obj) == PSCM_S_EOF) {
            printf("#<eof>");
        } else if (unum(obj) == PSCM_S_UNSPECIFIED) {
            printf("#<unspecified>");
        } else if (unum(obj) == PSCM_S_UNBOUND) {
            printf("#<unbound>");
        }
    } else {
        printf("#<?>");
    }
}

pscheme_fn(scheme$$base$$write) {
    pscheme_start();
    assert(pscheme_nargs() == 1);

    pscheme_write(pscheme_arg(0));
    pscheme_return(PSCM_UNSPECIFIED);
}
