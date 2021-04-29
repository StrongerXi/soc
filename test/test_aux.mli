open Pervasives

(** [check_and_output_str str ref_path output_path] checks whether [str] matches
    the content of the file at [ref_path], and (over)writes [str] to
    [output_path] *)
val check_and_output_str : string -> string -> string -> unit

(** [check_set expects actuals] makes sure
    - [expects] and [actuals] contain the same # of elements
    - All elements from [expects] are in [actuals] *)
val check_set : 'a list -> 'a Set.t -> unit

(** [check_set [(k1, v1), ..., (kn, vn)] map] makes sure
    - [map] contains each [ki] as key and nothing else
    - [ki] maps to [vi] in [map] *)
val check_map : ('k * 'v) list -> ('k, 'v) Map.t -> unit

(** [check_set expects actuals] makes sure
    - [expects] and [actuals] contain the same # of elements
    - All elements from [expects] are in [actuals] 
    NOTE intentionally separated from [check_set] because I want to use the
    right [mem] function for testing. *)
val check_unordered_list : 'a list -> 'a list -> unit
