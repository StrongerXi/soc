open Pervasives

(* This module stitches different phases of the compiler together, for easier
 * testing or building the actual compiler program.
 *
 * [X_file path] runs all the compiler phases up till [X] for the file at
 * [path]; it returns the target representation or formatted error string *)

val lex_file : string -> (Token.t list, string) result
val parse_file : string -> (Ast.structure, string) result
val type_file  : string -> (Ast.structure, string) result
val cir_file  : string -> (Cir.prog, string) result
val lir_file  : string -> (Lir.prog, string) result

(** Wire up instruction selection and register allocation *)
val lir_to_x86 : Lir.prog -> X86.func X86.prog
val lir_to_arm64 : Lir.prog -> Arm64.func Arm64.prog
