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
        array[i/(sizeof(size_t)*CHAR_BIT)] |= (1lu << (i % (sizeof(size_t)*CHAR_BIT)));
    } else {
        array[i/(sizeof(size_t)*CHAR_BIT)] &= ~(1lu << (i % (sizeof(size_t)*CHAR_BIT)));
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

static void *try_allocate_cell(struct cell_region *region) {
    if (region == NULL) {
        return NULL;
    }

    size_t i = first_free(region->allocated, region->search_off, CELL_REGION_OBJS);
    if (i == CELL_REGION_OBJS) {
        return try_allocate_cell(region->next);
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
    void *ptr = try_allocate_cell(cell_region);
    if (ptr != NULL)
        return ptr;

    //pscheme_collect_garbage();
    //ptr = try_allocate_cell(cell_region);
    //if (ptr != NULL)
    //    return ptr;

    cell_region = make_cell_region(cell_region);
    ptr = try_allocate_cell(cell_region);
    assert(ptr != NULL);
    return ptr;
}

#define BLOCK_REGION_BYTES (1024*1024)

struct block {
    struct block *next;
    size_t length;
    char data[] __attribute__((aligned (16)));
};

struct block_region {
    struct block_region *next;
    struct block *freelist;
    struct block *alloclist;
    size_t uninit_off;
    char data[BLOCK_REGION_BYTES] __attribute__((aligned (16)));
};

static struct block *try_allocate_block_freelist(struct block **block, size_t len) {
    if (*block == NULL) {
        return NULL;
    }

    // TODO: be smarter than this.
    if ((*block)->length >= len) {
        struct block *sblock = *block;
        *block = (*block)->next; // delete from freelist
        return sblock;
    }

    return try_allocate_block_freelist(&((*block)->next), len);
}

static void *try_allocate_block(struct block_region *region, size_t len) {
    if (region == NULL) {
        return NULL;
    }

    struct block *block = try_allocate_block_freelist(&(region->freelist), len);

    if (block == NULL) {
        if (len > BLOCK_REGION_BYTES - region->uninit_off - sizeof(struct block)) {
            return try_allocate_block(region->next, len);
        }

        block = (void *)(region->data + region->uninit_off);
        block->length = len;
        region->uninit_off += sizeof(struct block) + len;
        if (region->uninit_off % 16 != 0) {
            // properly re-align.
            region->uninit_off += (16 - (region->uninit_off % 16));
        }
    }

    block->next = region->alloclist;
    region->alloclist = block;
    return block->data;
}

static struct block_region *make_block_region(struct block_region *next) {
    struct block_region *region = malloc(sizeof(struct block_region));
    region->next = next;
    region->freelist = NULL;
    region->alloclist = NULL;
    region->uninit_off = 0;

    return region;
}

static struct block_region *block_region = NULL;

void *pscheme_allocate_block(size_t len) {
    void *ptr;
    ptr = try_allocate_block(block_region, len);
    if (ptr != NULL)
        return ptr;

    //pscheme_collect_garbage();
    //ptr = try_allocate_block(block_region, len);
    //if (ptr != NULL)
    //    return ptr;

    block_region = make_block_region(block_region);
    ptr = try_allocate_block(block_region, len);
    assert(ptr != NULL);
    return ptr;
}

static void clear_allocated_bit(struct cell_region *region) {
    if (region == NULL) {
        return;
    }

    region->search_off = 0;
    memset(region->allocated, 0, sizeof(region->allocated));

    clear_allocated_bit(region->next);
}

static struct cell_region *find_cell_region(struct cell_region *region, struct pscheme_cons_cell *ptr) {
    // find the region the cell is in.
    struct cell_region *r = region;
    for (; r != NULL; r = r->next) {
        if (ptr >= r->cells && ptr < r->cells + CELL_REGION_OBJS) {
            break;
        }
    }
    return r;
}

static struct block_region *find_block_region(struct block_region *region, void *ptr) {
    char *cptr = ptr;
    struct block_region *r = region;
    for (; r != NULL; r = r->next) {
        if (cptr >= r->data && cptr < r->data + BLOCK_REGION_BYTES) {
            break;
        }
    }
    return r;
}

static void scan_object(pscheme_t obj) {
    if (tag(obj) == PSCM_T_CONS) {
        struct pscheme_cons_cell *cell = ptr(obj);

        struct cell_region *r = find_cell_region(cell_region, cell);
        if (r == NULL)
            return;

        // mark the cell as allocated.
        set_bit(r->allocated, cell - r->cells, true);

        scan_object(cell->car);
        scan_object(cell->cdr);
    } else if (tag(obj) == PSCM_T_CLOSURE) {
        pscheme_t *closure = ptr(obj);

        struct block_region *r = find_block_region(block_region, closure);
        if (r == NULL)
            return;

        struct block *block = (void *)(((char *)closure) - sizeof(struct block));

        for (size_t i = 1; i < (block->length / sizeof(pscheme_t)); ++i) {
            scan_object(closure[i]);
        }

    } else if (tag(obj) == PSCM_T_STRING || tag(obj) == PSCM_T_SYMBOL) {
        // TODO: actually GC blocks
    }
}

static void scan_range(pscheme_t *start, pscheme_t *end) {
    for (pscheme_t *i = start; i < end; ++i) {
        scan_object(*i);
    }
}

static pscheme_t *stack_bottom(void) {
    static pscheme_t *bottom = NULL;
    if (bottom == NULL) {
        uintptr_t sb;
        FILE *statfp = fopen("/proc/self/stat", "r");
        assert(statfp != NULL);
        fscanf(statfp,
               "%*d %*s %*c %*d %*d %*d %*d %*d %*u "
               "%*u %*u %*u %*u %*u %*u %*d %*d "
               "%*d %*d %*d %*d %*u %*u %*d "
               "%*u %*u %*u %lu", &sb);
        bottom = (pscheme_t *) sb;

        fclose(statfp);
    }
    return bottom;
}

extern pscheme_t etext, edata, end;

void pscheme_collect_garbage(void) {
    pscheme_t stack_top;

    clear_allocated_bit(cell_region);
    scan_range(&etext, &end);
    scan_range(&stack_top, stack_bottom());
}

static void calc_cells(struct cell_region *region, size_t *regions, size_t *total, size_t *used) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += CELL_REGION_OBJS;
    for (size_t i = 0; i < sizeof(region->allocated)/sizeof(size_t); ++i) {
        *used += __builtin_popcount(region->allocated[i]);
    }

    calc_cells(region->next, regions, total, used);
}

static void calc_blocks(struct block_region *region, size_t *regions, size_t *total, size_t* objects, size_t *bytes) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += BLOCK_REGION_BYTES;
    for (struct block *b = region->alloclist; b != NULL; b = b->next) {
        *objects += 1;
        *bytes += b->length;
    }

    calc_blocks(region->next, regions, total, objects, bytes);
}

void pscheme_print_gc_stats(void) {
    size_t cell_regions = 0, total_cells = 0, used_cells = 0;
    calc_cells(cell_region, &cell_regions, &total_cells, &used_cells);

    size_t block_regions = 0, total_block_bytes = 0, used_block_objects = 0, used_block_bytes = 0;
    calc_blocks(block_region, &block_regions, &total_block_bytes, &used_block_objects, &used_block_bytes);

    // TODO: stderr causes segfault for some reason
    fprintf(stdout, "--- GC STATS ---\n");
    fprintf(stdout, "cell regions: %lu\n", cell_regions);
    fprintf(stdout, "total cells: %lu\n", total_cells);
    fprintf(stdout, "allocated cells: %lu\n\n", used_cells);
    fprintf(stdout, "block regions: %lu\n", block_regions);
    fprintf(stdout, "total bytes: %lu\n", total_block_bytes);
    fprintf(stdout, "allocated objects: %lu\n", used_block_objects);
    fprintf(stdout, "allocated bytes: %lu\n\n", used_block_bytes);
    fprintf(stdout, "total bytes: %lu\n", used_block_bytes + sizeof(struct pscheme_cons_cell)*used_cells);
}
