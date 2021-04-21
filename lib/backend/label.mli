open Pervasives

(** A [t] represents a label in low level representations *)
type t

(** A [manager] can generate unique [t] and associate string with [t] *)
type manager


(** An [init_manager] is a bare minimum [manager] *)
val init_manager : manager

(** [get_or_gen manager s] retrieves the [t] bound to [s].
    Return [None] if [s] is not bound in [manager]. *)
val get : manager -> string -> t option

(** [gen manager prefix] generates a unique [t] that starts with [prefix]
    and won't bind it with any string in output [manager].  
    ASSUME [prefix] doesn't contain space. *)
val gen : manager -> string -> (manager * t)

(** [gen_and_bind manager name] is like [gen manager], but output manager
    binds [name] with the returned [t] *)
val gen_and_bind : manager -> string -> (manager * t)

(** [get_native name] returns a label for externally defined function.
    It's guaranteed to be unique if [name] is unique *)
val get_native : string -> t

(** [to_epilogue t] returns a label that will start the epilogue portion of the
   function represented by [t]. Guaranteed to be unique. *)
val to_epilogue : t -> t

(** [is_native t] returns true iff [t] is created via [get_native] *)
val is_native : t -> bool

(** [to_string t] returns a string representation of [t] *)
val to_string : t -> string

val compare : t -> t -> int
