#include "common.h"

/**
 * A VALUE is one of
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
VALUE SOML_TRUE = 0x11;
VALUE SOML_FALSE = 0x01;


VALUE tag_int(int n) {
  return (n << 1) + 1;
}

int is_int(VALUE v) {
  return v & 0x1;
}

int is_obj(VALUE v) {
  return ~is_int(v);
}
