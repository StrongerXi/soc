#include "common.h"
#include "stdlib.h"
#include "stdio.h"

/**
 * A value is one of
 * - tagged integer
 * - tagged object
 * 
 * The tag is the least significant bit, 0 for object, 1 for integer.
 *
 * Object is one of
 * - closure
 *
 * NOTE this representation must synch up with the Lir module in soc compiler.
 */

// can't use `tag_int` here, make sure representation synchs up.
const value SOML_TRUE  = 0x11;
const value SOML_FALSE = 0x01;


value tag_int(int n) {
  return (n << 1) + 1;
}

int untag_int(value n) {
  if (! is_int(n)) {
    fprintf(stderr, "[common:untag_int] Given value is not an integer\n");
    exit(1);
  }
  return (n >> 1);
}

int is_int(value v) {
  return v & 0x1;
}

int is_obj(value v) {
  return ~is_int(v);
}
