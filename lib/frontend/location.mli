
(** A [t] represents a (row, column) location in some file *)
type t

(** [create row column] creates a (row, column) location *)
val create : int -> int -> t

(** [to_string t] returns a string representation of [t] *)
val to_string : t -> string

(** [skip_line t] is [t] skipping to the beginning of next line *)
val skip_line : t -> t
