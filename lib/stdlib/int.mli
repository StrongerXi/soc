open Pervasives

(** [max n1 n2] returns the larger of [n1] and [n2] *)
val max : int -> int -> int

(** [min n1 n2] returns the smaller of [n1] and [n2] *)
val min : int -> int -> int

(** [compare n1 n2] returns
    - negative if [n1 > n2]
    - 0        if [n1 = n2]
    - positive otherwise (n1 < n2) *)
val compare : int -> int -> int

(** [to_string n] returns a string representation of [n] *)
val to_string : int -> string

(** [of_string_opt s] returns an integer representation of [s],
    or [None] if [s] doesn't represent a valid integer *)
val of_string_opt : string -> int option
