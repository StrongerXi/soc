
(** [max n1 n2] returns the larger of [n1] and [n2] *)
val max : int -> int -> int

(** [min n1 n2] returns the smaller of [n1] and [n2] *)
val min : int -> int -> int

(** [ceil_div n1 n2] returns [n1 / n2] and rounds up the answer *)
val ceil_div : int -> int -> int

(** (n + [offset_to_align n alignment]) mod [alignment] = 0 
    NOTE
    1. Output is always the minimal non-negative solution.
    2. Undefined behavior if [n < 0] or [alignment <= 0]. *)
val offset_to_align : int -> int -> int

(** [compare n1 n2] returns
    - negative if [n1 > n2]
    - 0        if [n1 = n2]
    - positive otherwise (n1 < n2) *)
val compare : int -> int -> int

(** [to_string n] returns a string representation of [n] *)
val to_string : int -> string
