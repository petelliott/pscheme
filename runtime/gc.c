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
#define REGION_BYTES 1024
#else
#define REGION_BYTES (1024*1024*64)
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

struct slab_region {
    struct slab_region *next;
    size_t search_off;
    size_t size;
    size_t allocated[(REGION_BYTES/16)/(sizeof(size_t)*CHAR_BIT)];
    char data[REGION_BYTES] __attribute__((aligned (16)));
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

static void *try_allocate_slab(struct slab_region *region) {
    if (region == NULL) {
        return NULL;
    }

    size_t i = first_free(region->allocated, region->search_off, REGION_BYTES / region->size);
    if (i == (REGION_BYTES/region->size)) {
        return try_allocate_slab(region->next);
    }

    set_bit(region->allocated, i, 1);

    region->search_off = i + 1;
    void *slab = region->data + (i * region->size);
    memset(slab, 0, region->size);
    return slab;
}

static struct slab_region *make_slab_region(struct slab_region *next, size_t size) {
    assert(next == NULL || next->size == size);

    struct slab_region *region = malloc(sizeof(struct slab_region));
    region->next = next;
    region->search_off = 0;
    region->size = size;

    memset(region->allocated, 0, sizeof(region->allocated));

    return region;
}

#define N_SLAB_ALLOCATORS 3

static struct slab_region *slab_regions[N_SLAB_ALLOCATORS] = {0};
static struct rangetable slab_region_table = NEW_RANGETABLE;

static void *allocate_slab(size_t size) {
    size_t slabi = __builtin_ctz(size) - 4;
    assert(slabi < N_SLAB_ALLOCATORS);
    assert(slab_regions[slabi] == NULL || slab_regions[slabi]->size == size);

    void *ptr;
    if (slab_regions[slabi] != NULL) {
        ptr = try_allocate_slab(slab_regions[slabi]);
        if (ptr != NULL)
            return ptr;

        do_automatic_collection();
        ptr = try_allocate_slab(slab_regions[slabi]);
        if (ptr != NULL)
            return ptr;
    }

    slab_regions[slabi] = make_slab_region(slab_regions[slabi], size);
    rt_insert(&slab_region_table,
              (size_t) slab_regions[slabi]->data,
              (size_t) (slab_regions[slabi]->data + REGION_BYTES),
              slab_regions[slabi]);

    ptr = try_allocate_slab(slab_regions[slabi]);
    assert(ptr != NULL);
    return ptr;
}

void *pscheme_allocate_cell(void) {
    return allocate_slab(sizeof(struct pscheme_cons_cell));
}

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
    struct rangetable block_table;
    struct block *free_cursor;
    size_t uninit_off;
    char data[REGION_BYTES] __attribute__((aligned (16)));
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

    if (REGION_BYTES < region->uninit_off + sizeof(struct block) ||
        len > REGION_BYTES - region->uninit_off - sizeof(struct block)) {

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
    rt_insert(&region->block_table, (size_t) block->data, (size_t)(block->data + block->length), block);

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
    region->block_table = NEW_RANGETABLE;
    region->free_cursor = NULL;
    region->uninit_off = 0;

    return region;
}

static struct block_region *block_region = NULL;
static struct rangetable block_region_table = NEW_RANGETABLE;

void *pscheme_allocate_block(size_t len) {
    for (size_t i = 0; i < N_SLAB_ALLOCATORS; ++i) {
        size_t slab_size = 1 << (i+4);
        if (len < slab_size)
            return allocate_slab(slab_size);
    }

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
    rt_insert(&block_region_table, (size_t) block_region->data, (size_t) (block_region->data + REGION_BYTES), block_region);
    ptr = try_allocate_block(block_region, len);
    assert(ptr != NULL);
    return ptr;
}

static struct slab_region *find_slab_region(struct pscheme_cons_cell *ptr) {
    return rt_find(&slab_region_table, (size_t) ptr);
}

static struct block_region *find_block_region(void *ptr) {
    return rt_find(&block_region_table, (size_t) ptr);
}

static void scan_object(pscheme_t obj) {
    void *p = ptr(obj);
    struct slab_region *sr;
    struct block_region *br;

    if ((sr = find_slab_region(p)) != NULL) {
        size_t slabi = ((char*)p - sr->data) / sr->size;
        if (!get_bit(sr->allocated, slabi)) {
            // mark the cell as allocated.
            set_bit(sr->allocated, slabi, true);

            pscheme_t *slab = (void *)(sr->data + (slabi * sr->size));
            for (size_t i = 0; i < sr->size / sizeof(pscheme_t); ++i) {
                scan_object(slab[i]);
            }
        }
    } else if ((br = find_block_region(p)) != NULL) {
        struct block *block = rt_find(&br->block_table, (size_t)p);
        if (block == NULL)
            return;
        assert(block->canary == CANARY_VALUE);

        pscheme_t *slots = (void *)block->data;

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

static void clear_allocated_bit(struct slab_region *region) {
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

    for (size_t i = 0; i < N_SLAB_ALLOCATORS; ++i) {
        clear_allocated_bit(slab_regions[i]);
    }
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

static void calc_slabs(struct slab_region *region, size_t *regions, size_t *total, size_t *used) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += REGION_BYTES / region->size;
    for (size_t i = 0; i < sizeof(region->allocated)/sizeof(size_t); ++i) {
        *used += __builtin_popcountl(region->allocated[i]);
    }

    calc_slabs(region->next, regions, total, used);
}

static void calc_blocks(
    struct block_region *region, size_t *regions, size_t *total,
    size_t* objects, size_t* fobjects, size_t *bytes
) {
    if (region == NULL)
        return;

    *regions += 1;
    *total += REGION_BYTES;
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
    fprintf(stderr, "--- GC STATS ---\n");

    size_t total_bytes = 0;
    size_t used_bytes = 0;

    for (size_t i = 0; i < N_SLAB_ALLOCATORS; ++i) {
        size_t nslab_regions = 0, total_slabs = 0, used_slabs = 0;
        calc_slabs(slab_regions[i], &nslab_regions, &total_slabs, &used_slabs);

        fprintf(stderr, "%lu-byte regions:           %lu\n", slab_regions[i]->size, nslab_regions);
        fprintf(stderr, "total %lu-byte slabs:       %lu\n", slab_regions[i]->size, total_slabs);
        fprintf(stderr, "allocated  %lu-byte slabs:  %lu\n\n", slab_regions[i]->size, used_slabs);

        used_bytes += slab_regions[i]->size * used_slabs;
        total_bytes += REGION_BYTES * nslab_regions;
    }

    size_t block_regions = 0, total_block_bytes = 0, used_block_objects = 0, free_block_objects = 0, used_block_bytes = 0;
    calc_blocks(block_region, &block_regions, &total_block_bytes, &used_block_objects, &free_block_objects, &used_block_bytes);

    total_bytes += total_block_bytes;
    used_bytes += used_block_bytes;

    fprintf(stderr, "block regions:             %lu\n", block_regions);
    fprintf(stderr, "total bytes:               %lu\n", total_block_bytes);
    fprintf(stderr, "allocated objects:         %lu\n", used_block_objects);
    fprintf(stderr, "free objects:              %lu\n", free_block_objects);
    fprintf(stderr, "allocated bytes:           %lu\n\n", used_block_bytes);
    fprintf(stderr, "collections:               %lu\n", garbage_collections);
    fprintf(stderr, "live bytes:                %lu\n\n", used_bytes);
    fprintf(stderr, "total bytes:               %lu\n\n", total_bytes);

    // canary checks
    check_canaries(block_region);
}
