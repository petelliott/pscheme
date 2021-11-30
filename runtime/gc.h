#ifndef PSCHEME_GC_H
#define PSCHEME_GC_H

#include "object.h"
#include <stddef.h>

void *pscheme_allocate_cell(void);
void *pscheme_allocate_block(size_t len);

#endif
