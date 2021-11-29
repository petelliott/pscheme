#include "calling.h"
#include "object.h"
#include <assert.h>

// this function makes heavy use of the systemv calling convention
pscheme_t pscm_internal_rest(pscheme_t *_pscheme_args, size_t normal_args) {
    pscheme_start();

    size_t nargs = pscheme_nargs() - 3;
    assert(nargs >= normal_args);

    pscheme_t list = PSCM_NIL;
    for (int i = nargs - 1; i >= (int)normal_args; --i) {
        list = pscheme_cons(pscheme_arg(i), list);
    }

    pscheme_return(list);
}
