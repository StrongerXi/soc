open Pervasives

(** [length s] returns the # of characters in [s] *)
val length : string -> int

(** [get s i] returns the [i]th character of [s] (starting from 0).
    Errors if [i] is out of bounds *)
val get : string -> int -> char

(** [append s1 s2] returns a new string with [s1] and [s2] appended together *)
val append : string -> string -> string

(** [compare s1 s2] compares 2 strings lexicalgraphically *)
val compare : string -> string -> int