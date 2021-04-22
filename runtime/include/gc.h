#ifndef _GC_H
#define _GC_H
#include "common.h"

void gc_init(unsigned int heap_size);

// ENSURE: returned address is aligned to 8 bytes.
// NOTE
// - no guarantee on data in the allocated region
// - external name must synch up with soc compiler's backend Constants module.
char* gc_alloc(unsigned int size) RUNTIME_EXTERNAL(mem_alloc);

#endif
