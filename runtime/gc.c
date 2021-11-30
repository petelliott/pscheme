#include "gc.h"
#include <stdlib.h>


void *pscheme_allocate_cell(void) {
    // TODO: write a garbage collector
    return malloc(sizeof(struct pscheme_cons_cell));
}

void *pscheme_allocate_block(size_t len) {
    // TODO: write a garbage collector
    return malloc(len);
}
