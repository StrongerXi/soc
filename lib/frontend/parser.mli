open Pervasives

type token_stream =
  { next  : unit -> Token.t option (* [None] on EOF *)
  ; where : unit -> Location.t     (* last examined location *)
  }

(** [parse stream] returns a parsed abstract syntax tree,
    or errors on invalid input. *)
val parse : token_stream -> Ast.structure
