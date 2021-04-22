#include "builtins.h"
#include "common.h"

VALUE equal(VALUE v1, VALUE v2) {
  // shallow equality for now; expand after adding interesting data types,
  // e.g., record, tuple, or variants.
  return (v1 == v2) ?  SOML_TRUE : SOML_FALSE;
}
