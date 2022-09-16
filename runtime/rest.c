#include "calling.h"
#include "object.h"
#include <stdarg.h>

pscheme_t pscheme_internal_rest(va_list ap, size_t toget) {
    pscheme_t args[toget];

    for (size_t i = 0; i < toget; ++i) {
        args[i] = va_arg(ap, pscheme_t);
    }

    pscheme_t list = PSCM_NIL;
    for (long int i = toget - 1; i >= 0; --i) {
        list = pscheme_cons(args[i], list);
    }

    return list;
}
