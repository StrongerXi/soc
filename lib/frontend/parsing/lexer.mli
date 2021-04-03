open Pervasives

(** A [t] represents a lexer with its _mutable_ internal state *)
type t

(** [create filename] creates a lexer by reading from [filename].
    Error on invalid file. *)
val create : string -> t

(** Hook for client to catch lexing errors during downstream tasks *)
exception Lexer_error of Errors.lexer_error

(** [next t] returns the next token or error on invalid input.
    Update [t] accordingly in either case. *)
val next : t -> Token.t option

(** [next_loc t] returns the location of the next character [t] would examine *)
val next_loc : t -> Location.t
