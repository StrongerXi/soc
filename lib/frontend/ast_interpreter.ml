open Pervasives

(** Result of evaluating an AST expression *)
type value =
  | Int of int
  | Bool of bool
  | Closure of string list * Ast.expression * context
               (* arg names, body expr, enclosing context *)

(** A [context] is an immutable execution context for the interpreter *)
and context = (string, value) Map.t


let initial_context = Map.empty String.compare
;;

let interp_struct =
  failwith "TODO";
;;
