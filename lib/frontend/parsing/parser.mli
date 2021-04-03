open Pervasives

(** [parse tokens] returns a parsed abstract syntax tree from [tokens],
    or error on invalid input. Assume first token is the first in source. *)
val parse : Token.t list -> (Ast.structure, Errors.parser_error) result
