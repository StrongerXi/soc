open Pervasives

(** A [t] represents a lexer with its _mutable_ internal state *)
type t

(** [create filename] creates a lexer by reading from [filename].
    Error on invalid file. *)
val create : string -> t

(** [next t] returns the next token and update [t] accordingly.
    Error on invalid input. *)
val next : t -> Parser.token option

(** [next_loc t] returns the location of the next character [t] would examine *)
val next_loc : t -> Location.t
