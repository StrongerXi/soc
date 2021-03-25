open Pervasives

(** A [t] represents a lexer with its _mutable_ internal state *)
type t

(** [create filename] creates a lexer by reading from [filename].
    Error on invalid file. *)
val create : string -> t

(** [next t] returns the next token and a new lexer for the rest of the input.
    Error on invalid input. *)
val next : t -> Parser.token option
