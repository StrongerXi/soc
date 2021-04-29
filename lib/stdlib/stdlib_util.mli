open Pervasives

(* This module "lifts out" some functions to eliminate cyclic dependencies *)


(** [str_join_with [s1; ...; sn] sep] is [s1 ^ sep ^ ... ^ sep ^ sn] *)
val str_join_with : string list -> string -> string
