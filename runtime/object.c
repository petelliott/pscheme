#include "object.h"
#include "gc.h"
#include <assert.h>

pscheme_t pscheme_cons(pscheme_t a, pscheme_t b) {
    struct pscheme_cons_cell *cell_ptr = pscheme_allocate_cell();
    cell_ptr->car = a;
    cell_ptr->cdr = b;
    return ((uintptr_t)cell_ptr) | PSCM_T_CONS;
}

pscheme_t pscheme_car(pscheme_t obj) {
    assert(tag(obj) == PSCM_T_CONS);
    struct pscheme_cons_cell *cell = ptr(obj);
    return cell->car;
}

pscheme_t pscheme_cdr(pscheme_t obj) {
    assert(tag(obj) == PSCM_T_CONS);
    struct pscheme_cons_cell *cell = ptr(obj);
    return cell->cdr;
}
