#include "gc.h"
#include <stdlib.h>
#include <stdio.h>

// the bits that will be 0 in any memory address allocated by GC.
#define HEAP_ALIGNMENT 0b111

static int _did_init = 0;
static char* _next_free_start = 0;
static char* _heap_end = 0;


static void _check_init(const char* caller) {
  if (! _did_init) {
    fprintf(stderr, "[gc._check_init] failed. Caller = %s\n", caller);
    exit(1);
  }
}

static char* _align_ptr(char* ptr) {
  return (char*) ((uint64_t) ptr + HEAP_ALIGNMENT & ~HEAP_ALIGNMENT);
}


void gc_init(unsigned int heap_size) {
  _did_init = 1;
  // extra space for potential alignment
  char* heap = (char*) malloc(heap_size + HEAP_ALIGNMENT);
  if (heap == NULL) {
    fprintf(stderr,
        "[gc_init] failed to allocate heap with size %d\n", heap_size);
    exit(1);
  }

  char* aligned_heap_start = _align_ptr(heap);
  _next_free_start = aligned_heap_start;
  _heap_end = _next_free_start + heap_size;
}


char* gc_alloc(unsigned int size) {
  _check_init("gc_alloc");
  char* alloced = _next_free_start;
  _next_free_start = _align_ptr(_next_free_start + size);;
  if (_next_free_start > _heap_end) {
    fprintf(stderr, "[gc_alloc] Ran out of heap memory\n");
    exit(1);
  }
  return alloced;
}
