open Pervasives

(** [ok v] is [Ok v]. *)
val ok : 'a -> ('a, 'e) result

(** [error e] is [Error e]. *)
val error : 'e -> ('a, 'e) result

(** [bind r f] is [f v] if [r] is [Ok v] and [r] if [r] is [Error _]. *)
val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

(** [join rr] is [r] if [rr] is [Ok r] and [rr] if [rr] is [Error _]. *)
val join : (('a, 'e) result, 'e) result -> ('a, 'e) result

(** [map f r] is [Ok (f v)] if [r] is [Ok v] and [r] if [r] is [Error _]. *)
val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
