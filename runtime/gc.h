#ifndef PSCHEME_GC_H
#define PSCHEME_GC_H

#include "object.h"
#include <stddef.h>

pscheme_t pscheme_allocate_cell(enum pscheme_tags tag);
void *pscheme_allocate_block(size_t len);

#endif
