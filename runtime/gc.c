#include "gc.h"
#include <stdlib.h>


pscheme_t pscheme_allocate_cell(enum pscheme_tags tag) {
    // TODO: write a garbage collector
    void *ptr = malloc(sizeof(struct pscheme_cons_cell));
    return (pscheme_t)ptr | tag;
}

void *pscheme_allocate_block(size_t len) {
    // TODO: write a garbage collector
    return malloc(len);
}
