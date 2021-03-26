#include "object.h"
#include "gc.h"
#include <assert.h>

pscheme_t pscheme_cons(pscheme_t a, pscheme_t b) {
    pscheme_t cell = pscheme_allocate_cell(PSCM_T_CONS);
    struct pscheme_cons_cell *cell_ptr = ptr(cell);
    cell_ptr->car = a;
    cell_ptr->cdr = b;
    return cell;
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
