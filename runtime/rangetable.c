#include "rangetable.h"
#include <stdlib.h>

#define MAX(a,b) (((a) > (b))? (a) : (b))

static void ensure_cap(struct rangetable *table) {
    if (table->cap > table->len) return;

    size_t new_cap = MAX(table->cap * 2, 32);
    table->entries = realloc(table->entries, new_cap * sizeof(struct range));
    table->cap = new_cap;
}

static int compare(const void *va, const void *vb) {
    const struct range *a = va;
    const struct range *b = vb;
    if (a->end < b->start) {
        return -1;
    } else if (a->start >= b->end) {
        return 1;
    } else {
        return 0;
    }
}

void rt_insert(struct rangetable *table, size_t start, size_t end, void *ptr) {
    ensure_cap(table);
    table->entries[table->len] = (struct range){ .start = start, .end = end, .ptr = ptr };
    table->len++;

    // TODO: could be optimized further to a single memmove.
    for (size_t i = table->len - 1; i > 0; --i) {
        // extremley fast for most likely case, where each insertion is after the last
        if (table->entries[i].start >= table->entries[i-1].end)
            break;

        struct range tmp = table->entries[i];
        table->entries[i] = table->entries[i-1];
        table->entries[i-1] = tmp;
    }
}

void *rt_find(struct rangetable *table, size_t point) {
    struct range key = { .start = point, .end = point, .ptr = NULL };
    struct range *r = bsearch(&key, table->entries, table->len, sizeof(struct range), compare);
    if (r == NULL) return r;
    return r->ptr;
}
