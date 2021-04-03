
(** A [t] represents a (row, column) location in some file
    start with (1, 1), but not enforced during creation. *)
type t

(** [create row column] creates a (row, column) location *)
val create : int -> int -> t

(** [to_string t] returns a string representation of [t] *)
val to_string : t -> string

(** [advance t] moves [t] 1 unit towards the right *)
val advance : t -> t

(** [skip_line t] is [t] skipping to the beginning of next line *)
val skip_line : t -> t
