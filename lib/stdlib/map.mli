open Pervasives

(** A [key value t] represents an immutable one-to-one map from items of type
    [key] to items of type [value].
    NOTE undefined behavior to use two ['e t] with different underlying
    comparison functions (passed in from [empty]). *)
type ('k, 'v) t

(** [empty cmp] creates an empty map assuming [cmp x y] returns
    - negative if [x] is considered less than [y]
    - 0        if [x] is considered equal to [y]
    - positive if [x] is considered greater than [y]
    in a consistent manner *)
val empty : ('k -> 'k -> int) -> ('k, 'v) t

(** [is_empty t] returns true iff [t] contains no elements *)
val is_empty : ('k, 'v) t -> bool

(** [size t] returns the # of key-value pairs in [t] *)
val size : ('k, 'v) t -> int

(** [get key t] returns [Some value] if [key] is mapped to [value] in [t],
    and [None] otherwise. *)
val get : 'k -> ('k, 'v) t -> 'v option

(** [add key value t] returns a new map with [key] mapped to [value],
    overwriting existing [key], if any. *)
val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

(** [remove key t] returns a map where [key] doesn't map to anything *)
val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

(** [map f t] returns a new map where each value [v] in [t] becomes [f v] *)
val map : ('v -> 'b) -> ('k, 'v) t -> ('k, 'b) t

(** [mapi f t] returns a new map where for each key-value pair [(k, v)] in [t],
    the value becomes [f k v] *)
val mapi : ('k -> 'v -> 'b) -> ('k, 'v) t -> ('k, 'b) t

(** [fold f t a] is (f vN ... (f v1 a)...), where v1 ... vN are all the value
    in [t] *)
val fold : ('v -> 'b -> 'b) -> ('k, 'v) t -> 'b -> 'b

(** [fold f t a] is (f kN vN ... (f k1 v1 a)...), where (k1, v1) ... (kN vN) are
    all the keys value pairs in [t] *)
val foldi : ('k -> 'v -> 'b -> 'b) -> ('k, 'v) t -> 'b -> 'b

(** [to_string f g t] returns a string representation of the [t], using [f] to
    format individual keys, and [g] for elements. *)
val to_string : ('k -> string) -> ('v -> string) -> ('k, 'v) t -> string

(** [get_key_set t] returns all the keys present in [t] as a set *)
val get_key_set : ('k, 'v) t -> 'k Set.t
