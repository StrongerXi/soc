#ifndef _COMMON_H
#define _COMMON_H

/**
 * Usage:
 *    extern f() RUNTIME_EXTERNAL(soml_f);
 * 
 * 1. If `f` is defined externally by the soc compiler as `soml_f`
 *    this macro allows us to use `f` inside C runtime.
 *
 * 2. If `f` is defined internally by the C runtime,
 *    this macro allows output of soc compiler to use `f` as `soml_f`.
 *
 * NOTE this must synch up with the label module in soc compiler.
 */
#define RUNTIME_EXTERNAL(name) asm(#name "_native")

// Generic soml value
#define VALUE unsigned int

VALUE SOML_TRUE;
VALUE SOML_FALSE;

VALUE tag_int(int);
int is_int(VALUE);
int is_obj(VALUE);

#endif
