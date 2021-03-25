
(** [is_uppercase ch] returns true iff [ch] is among 'A' ... 'Z' *)
val is_uppercase : char -> bool

(** [is_lowercase ch] returns true iff [ch] is among 'a' ... 'z' *)
val is_lowercase : char -> bool

(** [is_alpha ch] is [is_uppercase ch || is_lowercase ch] *)
val is_alpha : char -> bool

(** [is_num ch] returns true iff [ch] is among '0' ... '9' *)
val is_num : char -> bool

(** [is_alphanum ch] is [is_alpha ch || is_num ch] *)
val is_alphanum : char -> bool

(** [compare ch1 ch2] returns
    - positive if [ch1 > ch2]
    - 0        if [ch1 = ch2]
    - negative otherwise (ch1 < ch2)
    where comparison is based on their ascii code. *)
val compare : char -> char -> int

(** [to_string ch] returns a string representation of [ch] *)
val to_string : char -> string
