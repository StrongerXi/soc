open Pervasives

(** A [t] represents some temporary register *)
type t

(** A [manager] can generate unique [t] and associate string with [t] *)
type manager


(** An [init_manager] is a bare minimum [manager] *)
val init_manager : manager

(** [get_or_gen manager s] retrieves the [t] bound to [s].
    Return [None] if [s] is not bound in [manager]. *)
val get : manager -> string -> t option

(** [gen manager] generates a unique [t] and won't bind it with any string
    in output [manager] *)
val gen : manager -> (manager * t)

(** [gen_and_bind manager name] is like [gen manager], but output manager
    binds [name] with the returned [t] *)
val gen_and_bind : manager -> string -> (manager * t)

(** [to_string t] returns a string representation of [t] *)
val to_string : t -> string

(** An empty set of [t], with _some_ ordering. *)
val empty_set : t Set.t

(** [empty_map ()] is an empty map of [t], with _some_ ordering. 
    It's made a function to support parameterized key type. *)
val empty_map : unit -> (t, 'v) Map.t
