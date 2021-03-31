open Pervasives

(** [parse lexer] returns a parsed abstract syntax tree,
    or error on invalid input. *)
val parse : Lexer.t -> (Ast.structure, Errors.parser_error) result
