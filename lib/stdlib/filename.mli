open Pervasives

(** [split_extension s] splits [s] by the last ".", and return the substring
    before and after the ".".
    If no "." is found, return [None] for the second substring. *)
val split_extension : string -> string * string option

