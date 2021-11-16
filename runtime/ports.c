#include <runtime/calling.h>
#include <runtime/object.h>
#include <assert.h>
#include <stdio.h>

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
    } else if (tag(obj) == PSCM_T_CHAR) {
        printf("#\\%c", (char)num(obj));
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
