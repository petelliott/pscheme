#include "gc.h"
#include <stdlib.h>
#include "object.h"
#include <limits.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#define CELL_REGION_OBJS (1024*1024)

struct cell_region {
    struct cell_region *next;
    size_t search_off;
    size_t allocated[CELL_REGION_OBJS/(sizeof(size_t)*CHAR_BIT)];
    struct pscheme_cons_cell cells[CELL_REGION_OBJS];
};


/*
static bool get_bit(size_t *array, size_t i) {
    return array[i/(sizeof(size_t)*8)]
        & (1lu << (i % (sizeof(size_t)*8)));
}
*/

static void set_bit(size_t *array, size_t i, bool val) {
    if (val) {
        array[i/(sizeof(size_t)*8)] |= (1lu << (i % (sizeof(size_t)*8)));
    } else {
        array[i/(sizeof(size_t)*8)] &= ~(1lu << (i % (sizeof(size_t)*8)));
    }
}

// gets the index of the first free 0 bit
static size_t first_free(const size_t *array, size_t start, size_t len) {
    size_t start_block = start/(sizeof(size_t)*CHAR_BIT);
    for (size_t i = start_block; i < len/(sizeof(size_t) * CHAR_BIT); ++i) {
        size_t block = array[i];
        if (block != 0xfffffffffffffffflu) {
            for (size_t j = 0; j < sizeof(size_t) * CHAR_BIT; ++j) {
                if (!(block & (1lu << j))) {
                    return i*sizeof(size_t)*CHAR_BIT + j;
                }
            }
        }
    }
    return len;
}

static void *allocate_or_null(struct cell_region *region) {
    if (region == NULL) {
        return NULL;
    }

    size_t i = first_free(region->allocated, region->search_off, CELL_REGION_OBJS);
    if (i == CELL_REGION_OBJS) {
        return allocate_or_null(region->next);
    }

    set_bit(region->allocated, i, 1);

    region->search_off = i + 1;
    return &(region->cells[i]);
}

static struct cell_region *make_cell_region(struct cell_region *next) {
    struct cell_region *region = malloc(sizeof(struct cell_region));
    region->next = next;
    region->search_off = 0;

    memset(region->allocated, 0, sizeof(region->allocated));

    return region;
}

static struct cell_region *cell_region = NULL;

void *pscheme_allocate_cell(void) {
    void *ptr = allocate_or_null(cell_region);
    if (ptr == NULL) {
        // TODO: collect garbage
        cell_region = make_cell_region(cell_region);
        ptr = allocate_or_null(cell_region);
        assert(ptr != NULL);
    }
    return ptr;
}

void *pscheme_allocate_block(size_t len) {
    // TODO: write a garbage collector
    return malloc(len);
}
