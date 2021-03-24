open Pervasives

(** [some x] is [Some x] *)
val some : 'a -> 'a option

(** [value o dft] is v if [o = Some v] and [dft] otherwise. *)
val value : 'a option -> 'a -> 'a

(** [bind o f] is [f v] if [o = Some v] and [None] otherwise *)
val bind : 'a option -> ('a -> 'b option) -> 'b option

(** [join oo] is [Some v] if [oo = Some (Some v)] and [None] otherwise. *)
val join : 'a option option -> 'a option

(** [map f o] is [Some (f v)] is [o = Some v] and [None] otherwise *)
val map : ('a -> 'b) -> 'a option -> 'b option

(** [iter f o] is [f v] if [o = Some v] and () otherwise. *)
val iter : ('a -> unit) -> 'a option -> unit
