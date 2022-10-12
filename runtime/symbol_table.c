#include "object.h"
#include <string.h>

extern pscheme_t __start_symbols, __stop_symbols;

pscheme_t pscheme_comptime_symbol_defined(pscheme_t s) {
    for (pscheme_t *i = &__start_symbols; i < &__stop_symbols; ++i) {
        if (strcmp(ptr(s), ptr(*i)) == 0) {
            return *i;
        }
    }
    return PSCM_F;
}
