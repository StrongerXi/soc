#ifndef _COMMON_H
#define _COMMON_H

/**
 * Usage:
 *    f() RUNTIME_EXTERNAL(soml_f);
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
typedef int value;

extern const value SOML_TRUE;
extern const value SOML_FALSE;

value tag_int(int);
int untag_int(value);
int is_int(value);
int is_obj(value);

#endif
