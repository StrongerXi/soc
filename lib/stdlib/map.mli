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
