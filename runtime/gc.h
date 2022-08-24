#ifndef PSCHEME_GC_H
#define PSCHEME_GC_H

#include <stddef.h>

void *pscheme_allocate_cell(void);
void *pscheme_allocate_block(size_t len);

void pscheme_collect_garbage(void);

void pscheme_print_gc_stats(void);

#endif
