#include "gc.h"
#include "rangetable.h"
#include "object.h"
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#define MINI_REGIONS 0

#if MINI_REGIONS
#define CELL_REGION_OBJS 64
#else
#define CELL_REGION_OBJS (1024*1024)
#endif


static bool autocollect = true;
void pscheme_set_automatic_collection(bool enabled) {
    autocollect = enabled;
}

bool pscheme_is_automatic_collection_enabled(void) {
    return autocollect;
}

static void do_automatic_collection(void) {
    if (autocollect) {
        pscheme_collect_garbage();
    }
}

static void print_block_metadata(void);

struct cell_region {
    struct cell_region *next;
    size_t search_off;
    size_t allocated[CELL_REGION_OBJS/(sizeof(size_t)*CHAR_BIT)];
    struct pscheme_cons_cell cells[CELL_REGION_OBJS];
};

static bool get_bit(size_t *array, size_t i) {
    return array[i/(sizeof(size_t)*8)]
        & (1lu << (i % (sizeof(size_t)*8)));
}

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
    memset(&(region->cells[i]), 0, sizeof(struct pscheme_cons_cell));
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
static struct rangetable cell_region_table = NEW_RANGETABLE;

void *pscheme_allocate_cell(void) {
    void *ptr;
    if (cell_region != NULL) {
        ptr = try_allocate_cell(cell_region);
        if (ptr != NULL)
            return ptr;

        do_automatic_collection();
        ptr = try_allocate_cell(cell_region);
        if (ptr != NULL)
            return ptr;
    }

    cell_region = make_cell_region(cell_region);
    rt_insert(&cell_region_table, (size_t) cell_region->cells, (size_t) (cell_region->cells + CELL_REGION_OBJS), cell_region);
    ptr = try_allocate_cell(cell_region);
    assert(ptr != NULL);
    return ptr;
}

#if MINI_REGIONS
#define BLOCK_REGION_BYTES 1024
#else
#define BLOCK_REGION_BYTES (1024*1024)
#endif

#define CANARY_VALUE 0x1ab691a42e3a2c1f

struct block {
    size_t canary;
    struct block *next;
    uint32_t length;
    bool free;
    char data[] __attribute__((aligned (16)));
};

struct block_region {
    struct block_region *next;
    struct block *list;
    struct block *free_cursor;
    size_t uninit_off;
    char data[BLOCK_REGION_BYTES] __attribute__((aligned (16)));
};

static struct block *try_allocate_block_freelist(struct block *block, size_t len) {
    if (block == NULL) {
        return NULL;
    }

    // TODO: be smarter than this.
    if (block->free && block->length >= len) {
        return block;
    }

    return try_allocate_block_freelist(block->next, len);
}

static struct block *try_allocate_free_block(struct block_region *region, size_t len) {
    if (region == NULL) {
        return NULL;
    }

    struct block *block = try_allocate_block_freelist(region->free_cursor, len);
    region->free_cursor = block;
    if (block == NULL) {
        block = try_allocate_free_block(region->next, len);
        if (block == NULL)
            return block;
    }

    memset(block->data, 0, block->length);
    return block;
}

static struct block *try_allocate_fresh_block(struct block_region *region, size_t len) {
    if (region == NULL) {
        return NULL;
    }

    if (BLOCK_REGION_BYTES < region->uninit_off + sizeof(struct block) ||
        len > BLOCK_REGION_BYTES - region->uninit_off - sizeof(struct block)) {

        return try_allocate_fresh_block(region->next, len);
    }

    struct block *block = (void *)(region->data + region->uninit_off);
    block->canary = CANARY_VALUE;
    size_t data_start = region->uninit_off + sizeof(struct block);
    region->uninit_off = data_start + len;
    if (region->uninit_off % 16 != 0) {
        // properly re-align.
        region->uninit_off += (16 - (region->uninit_off % 16));
    }
    block->length = region->uninit_off - data_start;

    block->next = region->list;
    region->list = block;

    return block;
}

static void *try_allocate_block(struct block_region *region, size_t len) {
    struct block *block = try_allocate_free_block(region, len);
    if (block == NULL) {
        block = try_allocate_fresh_block(region, len);
        if (block == NULL) {
            return NULL;
        }
    }

    memset(block->data, 0, block->length);
    block->free = false;
    return block->data;
}

static struct block_region *make_block_region(struct block_region *next) {
    struct block_region *region = malloc(sizeof(struct block_region));
    region->next = next;
    region->list = NULL;
    region->free_cursor = NULL;
    region->uninit_off = 0;

    return region;
}

static struct block_region *block_region = NULL;
static struct rangetable block_region_table = NEW_RANGETABLE;

void *pscheme_allocate_block(size_t len) {
    if (len <= sizeof(struct pscheme_cons_cell)) {
        return pscheme_allocate_cell();
    }

    void *ptr;
    if (block_region != NULL) {
        ptr = try_allocate_block(block_region, len);
        if (ptr != NULL)
            return ptr;

        do_automatic_collection();
        ptr = try_allocate_block(block_region, len);
        if (ptr != NULL)
            return ptr;
    }

    block_region = make_block_region(block_region);
    rt_insert(&block_region_table, (size_t) block_region->data, (size_t) (block_region->data + BLOCK_REGION_BYTES), block_region);
    ptr = try_allocate_block(block_region, len);
    assert(ptr != NULL);
    return ptr;
}

static struct cell_region *find_cell_region(struct cell_region *region, struct pscheme_cons_cell *ptr) {
    return rt_find(&cell_region_table, (size_t) ptr);
}

static struct block_region *find_block_region(struct block_region *region, void *ptr) {
    return rt_find(&block_region_table, (size_t) ptr);
}

static void scan_object(pscheme_t obj) {
    if (!is_ptr_obj(obj)) {
        return;
    }

    void *p = ptr(obj);
    struct cell_region *cr;
    struct block_region *br;

    if ((cr = find_cell_region(cell_region, p)) != NULL) {
        struct pscheme_cons_cell *cell = p;
        if (!get_bit(cr->allocated, cell - cr->cells)) {
            // mark the cell as allocated.
            set_bit(cr->allocated, cell - cr->cells, true);

            if (!is_leaf_obj(obj)) {
                scan_object(cell->car);
                scan_object(cell->cdr);
            }
        }
    } else if ((br = find_block_region(block_region, p)) != NULL) {
        pscheme_t *slots = p;

        struct block *block = (void *)(((char *)p) - sizeof(struct block));

        if (block->canary != CANARY_VALUE) {
            fprintf(stderr, "almost corrupted the heap. let me know if you hit this.\n");
            return;
        }

        if (block->free) {
            block->free = false;

            if (!is_leaf_obj(obj)) {
                for (size_t i = 1; i < (block->length / sizeof(pscheme_t)); ++i) {
                    scan_object(slots[i]);
                }
            }
        }
    }
}

static void clear_allocated_bit(struct cell_region *region) {
    if (region == NULL) {
        return;
    }

    region->search_off = 0;
    memset(region->allocated, 0, sizeof(region->allocated));

    clear_allocated_bit(region->next);
}

static void free_all_blocks(struct block_region *region) {
    for (struct block_region *r = region; r != NULL; r = r->next) {
        r->free_cursor = r->list;
        for (struct block *b = r->list; b != NULL; b = b->next) {
            b->free = true;
        }
    }
}

#define ALIGN(ptr, pwr) ((typeof(ptr))((((uintptr_t)(ptr)) >> (pwr)) << (pwr)))

static void scan_range(void *start, void *end) {
    for (pscheme_t *i = ALIGN(start, 3); (void *)i < end; ++i) {
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

static size_t garbage_collections = 0;

extern char etext, edata, end;

#define SCAN_REG(reg) {pscheme_t val; asm("movq %" reg ", %0" : "=r"(val)); scan_object(val);}

void pscheme_collect_garbage(void) {
    pscheme_t stack_top;

    clear_allocated_bit(cell_region);
    free_all_blocks(block_region);

    SCAN_REG("%rbx");
    SCAN_REG("%rsp");
    SCAN_REG("%rbp");
    SCAN_REG("%r12");
    SCAN_REG("%r13");
    SCAN_REG("%r14");
    SCAN_REG("%r15");
    scan_range(&etext, &end);
    scan_range(&stack_top, stack_bottom());

    ++garbage_collections;
}

static void calc_cells(struct cell_region *region, size_t *regions, size_t *total, size_t *used) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += CELL_REGION_OBJS;
    for (size_t i = 0; i < sizeof(region->allocated)/sizeof(size_t); ++i) {
        *used += __builtin_popcountl(region->allocated[i]);
    }

    calc_cells(region->next, regions, total, used);
}

static void calc_blocks(
    struct block_region *region, size_t *regions, size_t *total,
    size_t* objects, size_t* fobjects, size_t *bytes
) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += BLOCK_REGION_BYTES;
    for (struct block *b = region->list; b != NULL; b = b->next) {
        if (!b->free) {
            *objects += 1;
            *bytes += b->length;
        } else {
            *fobjects += 1;
        }
    }

    calc_blocks(region->next, regions, total, objects, fobjects, bytes);
}

struct blockmeta {
    int size;
    int fcount;
    int acount;
};

#define METADATA_LEN 1024

static void record_metadata(struct blockmeta *meta, size_t *moff, size_t len, bool free) {
    size_t i = 0;
    for (; i < *moff; ++i) {
        if (meta[i].size == len)
            break;
    }
    assert(i < METADATA_LEN);
    if (i == *moff) {
        ++*moff;
        meta[i].size = len;
        meta[i].fcount = 0;
        meta[i].acount = 0;
    }

    if (free) {
        meta[i].fcount++;
    } else {
        meta[i].acount++;
    }
}

static void scan_metadata(struct block_region *region, struct blockmeta *meta, size_t *moff) {
    if (region == NULL)
        return;

    for (struct block *b = region->list; b != NULL; b = b->next) {
        record_metadata(meta, moff, b->length, b->free);
    }

    scan_metadata(region->next, meta, moff);
}

static void print_block_metadata(void) {
    struct blockmeta metadata[1024];
    size_t moff = 0;
    scan_metadata(block_region, metadata, &moff);

    fprintf(stderr, "--- BLOCK ALLOCATOR STATS ---\n");
    for (size_t i = 0; i < moff; ++i) {
        fprintf(stderr, "%d\t%d/%d\n", metadata[i].size, metadata[i].fcount, metadata[i].acount);
    }
}

static void check_canaries(struct block_region *region) {
    if (region == NULL) return;

    for (struct block *b = region->list; b != NULL; b = b->next) {
        if (b->canary != CANARY_VALUE) {
            fprintf(stderr, "corrupted block: %p\n", b);
        }
    }

    check_canaries(region->next);
}

void pscheme_print_gc_stats(void) {
    size_t cell_regions = 0, total_cells = 0, used_cells = 0;
    calc_cells(cell_region, &cell_regions, &total_cells, &used_cells);

    size_t block_regions = 0, total_block_bytes = 0, used_block_objects = 0, free_block_objects = 0, used_block_bytes = 0;
    calc_blocks(block_region, &block_regions, &total_block_bytes, &used_block_objects, &free_block_objects, &used_block_bytes);

    fprintf(stderr, "--- GC STATS ---\n");
    fprintf(stderr, "cell regions:      %lu\n", cell_regions);
    fprintf(stderr, "total cells:       %lu\n", total_cells);
    fprintf(stderr, "allocated cells:   %lu\n\n", used_cells);
    fprintf(stderr, "block regions:     %lu\n", block_regions);
    fprintf(stderr, "total bytes:       %lu\n", total_block_bytes);
    fprintf(stderr, "allocated objects: %lu\n", used_block_objects);
    fprintf(stderr, "free objects:      %lu\n", free_block_objects);
    fprintf(stderr, "allocated bytes:   %lu\n\n", used_block_bytes);
    fprintf(stderr, "collections:       %lu\n", garbage_collections);
    fprintf(stderr, "total bytes:       %lu\n\n", used_block_bytes + sizeof(struct pscheme_cons_cell)*used_cells);

    // canary checks
    check_canaries(block_region);
}
