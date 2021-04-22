#include "builtins.h"
#include "common.h"
#include <stdio.h>

value equal(value v1, value v2) {
  // shallow equality for now; expand after adding interesting data types,
  // e.g., record, tuple, or variants.
  return (v1 == v2) ?  SOML_TRUE : SOML_FALSE;
}

value print(value v) {
  if (is_int(v)) {
    printf("int:<%d>\n", untag_int(v));
  } else {
    printf("<function>\n");
  }
  return v;
}
