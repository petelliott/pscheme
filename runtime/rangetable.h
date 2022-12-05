#ifndef PSCHEME_RANGETABLE_H
#define PSCHEME_RANGETABLE_H

#include <stddef.h>

struct range {
    size_t start;
    size_t end;
    void *ptr;
};

struct rangetable {
    size_t len;
    size_t cap;
    struct range *entries;
};

#define NEW_RANGETABLE (struct rangetable){ .len = 0, .cap = 0, .entries = NULL }

void rt_insert(struct rangetable *table, size_t start, size_t end, void *ptr);
void *rt_find(struct rangetable *table, size_t point);

#endif
