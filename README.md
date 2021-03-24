# SOC

A Subset of OCaml Compiler. 

The end goal is to compile all the OCaml portion of the codebase (excluding some
primitive functions, which will be written as part of the runtime in C).

## Why

From my experience, most students can build a compiler for some small language,
sometimes with type-checking, classes, lambdas, or even a rudimentary runtime.

However, there still exists a _huge_ gap between such "toy compilers" and
production compiler, such as [OCaml's](https://github.com/ocaml/ocaml).

I _think_ a self-compiling compiler would help bridge that gap, and
significantly help one understand a language.

As to _why OCaml_? I just love this language:).