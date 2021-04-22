#ifndef _BUILTINS_H
#define _BUILTINS_H
#include "common.h"

// external name must synch up with soc compiler's Cir module.
VALUE equal(VALUE, VALUE) RUNTIME_EXTERNAL(equal);

#endif
