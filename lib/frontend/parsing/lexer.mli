open Pervasives

(** [lex_file source] lexes [source] into a list of tokens (first one being
    first in the file) or return error on invalid input *)
val lex : string -> (Token.t list, Errors.lexer_error) result
